{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Magma (runClang, runClog, ToolOpts(..), ProjectOpts(..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.KeyMap
import Data.Aeson.Key
import Data.Maybe
import Data.Text (pack, unpack)
import Development.Shake
import Development.Shake.FilePath
import GHC.Generics
import System.Directory
import Text.CSV
import Text.Regex.TDFA
import qualified Data.Vector as Vector
import qualified Clang.CompilationDatabase as CDB

import Report
import ClangTidy
import Clog


data ProjectOpts = ProjectOpts {
  dir :: FilePath,
  groundTruth :: FilePath,
  includes :: [FilePath],
  outputDir :: FilePath
} deriving (Show, Generic)


data ToolOpts = ToolOpts {
  clangXargs :: [String],
  clogXargs :: [String],
  clogJar :: FilePath,
  clogProgramPath :: FilePath,
  clogUtilPath :: FilePath,
  clangFilter :: String,
  clogFilter :: String,
  groundTruthFilter :: String
  } deriving (Show, Generic)

instance FromJSON ProjectOpts
instance FromJSON ToolOpts
instance ToJSON ProjectOpts
instance ToJSON ToolOpts

acceptFile :: FilePath -> Bool
acceptFile f = f =~ ".*\\.c"

rules :: ToolOpts -> ProjectOpts -> Rules()
rules topts popts = do
  let compile_commands = popts.dir </> "compile_commands.json"
      patched_compile_commands = "compile_commands.json"
      sources_csv = "srcs.csv"
      includes = ("-I" ++) <$> popts.includes

  sources_csv %> \out -> do
    need [compile_commands]
    Just cdb <- liftIO (decodeFileStrict compile_commands :: IO (Maybe CDB.CompilationDatabase))
    let files = (\e -> [unpack (CDB.directory e) </> unpack (CDB.file e), "A"]) <$> Prelude.filter (acceptFile . unpack . CDB.file) cdb
    liftIO $ writeFile' out (printCSV files)

  patched_compile_commands %> \out -> do
    need [compile_commands]
    Just cdb <- liftIO (decodeFileStrict compile_commands :: IO (Maybe CDB.CompilationDatabase))
    let patched_cdb = (\e -> e { CDB.arguments = CDB.arguments e >>= \args -> return $ args ++ (pack <$> includes) }) <$> cdb
    liftIO $ encodeFile out patched_cdb

  ["clang.analysis.out", "clang.analysis.time"] &%> \[out, time] -> do
    need [patched_compile_commands]
    Just cdb <- liftIO (decodeFileStrict patched_compile_commands :: IO (Maybe CDB.CompilationDatabase))
    let files = (\e -> unpack (CDB.directory e) </> unpack (CDB.file e)) <$> Prelude.filter (acceptFile . unpack . CDB.file) cdb
    (CmdTime t) <- cmd (FileStdout out) (FileStderr "clang.analysis.err")
      "clang-tidy"
      "-p ."
      (topts.clangXargs ++ files)
    writeFile' time (show t)

  "clang.analysis.csv" %> \out -> do
    need ["clang.analysis.out"]
    ls <- readFileLines $ "clang.analysis.out"
    let reports = fmap (\r -> [file r
                              , show $ line r
                              , show $ col r
                              , desc r
                                ]) $
                  Prelude.filter (\r -> kind r == WarningReport && isJust (extractChecker r))
                  (processClangTidyOutput ls)
    writeFile' out (printCSV reports)

  ["clog.analysis.csv", "clog.analysis.time"] &%> \[_, time] -> do
    need [sources_csv, patched_compile_commands, topts.clogProgramPath, topts.clogJar]
    (CmdTime t) <- cmd (FileStdout "clog.analysis.out") (FileStderr "clog.analysis.err")
      "java" "-jar" topts.clogJar
      "-lang" "c4"
      "-S" sources_csv
      ["-Xclang=-p ."]
      "-D" "."
      topts.clogXargs
      topts.clogProgramPath
    writeFile' time (show t)

  ["clang.true.positive.csv", "clang.false.positive.csv", "clang.false.negative.csv"] &%> \[otp, ofp, ofn] -> do
    need ["ground.truth.csv", "clang.analysis.csv"]
    toolReport <- liftIO $ extractReportsCSV $ "clang.analysis.csv"
    groundReport <- liftIO $ do
      Right csv <- parseCSVFromFile "ground.truth.csv"
      return $ (\[cve, cwe, f, ls, le, _] -> Report f ((read ls)::Int) (0::Int) ((read le)::Int) (0::Int) cwe OtherReport)
        <$> Prelude.filter (/= [""]) csv
    let (tps, fps, fns) = compareReport classifyClangReport classifyMagmaReport topts.clangFilter topts.groundTruthFilter toolReport groundReport
        toCSVLine r = [file r, show $ line r, show $ endLine r, desc r]
    liftIO $ do
      writeFile' otp (printCSV $ toCSVLine <$> tps)
      writeFile' ofp (printCSV $ toCSVLine <$> fps)
      writeFile' ofn (printCSV $ toCSVLine <$> fns)

  ["clog.true.positive.csv", "clog.false.positive.csv", "clog.false.negative.csv"] &%> \[otp, ofp, ofn] -> do
    need ["ground.truth.csv", "clog.analysis.csv"]
    toolReport <- liftIO $ extractReportsCSV $ "clog.analysis.csv"
    groundReport <- liftIO $ do
      Right csv <- parseCSVFromFile "ground.truth.csv"
      return $ (\[cve, cwe, f, ls, le, _] -> Report f ((read ls)::Int) (0::Int) ((read le)::Int) (0::Int) cwe OtherReport)
        <$> Prelude.filter (/= [""]) csv
    let (tps, fps, fns) = compareReport classifyClogReport classifyMagmaReport topts.clogFilter topts.groundTruthFilter toolReport groundReport
        toCSVLine r = [file r, show $ line r, show $ endLine r, desc r]
    liftIO $ do
      writeFile' otp (printCSV $ toCSVLine <$> tps)
      writeFile' ofp (printCSV $ toCSVLine <$> fps)
      writeFile' ofn (printCSV $ toCSVLine <$> fns)


  "func.ranges.csv" %> \out -> do
    need [popts.groundTruth, sources_csv, patched_compile_commands]
    Just jsonGT <- liftIO (decodeFileStrict popts.groundTruth :: IO (Maybe Value))
    let tabularGT = (\(t0, t1, t2, t3) -> [t0, t1, t2, t3]) <$> flatten jsonGT
    writeFile' out (printCSV tabularGT)

  "ground.truth.csv" %> \_ -> do
    need [topts.clogUtilPath, "func.ranges.csv"]
    cmd (FileStdout "clog.util.out") (FileStderr "clog.util.err")
      "java" "-jar" topts.clogJar
      "-lang" "c4"
      "-S" sources_csv
      ["-Xclang=-p ."]
      topts.clogXargs
      topts.clogUtilPath

compareReport :: ReportClassifier --
              -> ReportClassifier --
              -> String -- report filter
              -> String  -- report filter
              -> [Report] -- tool reports
              -> [Report] -- ground truth reports
              -> ([Report], -- true positives
                  [Report], -- false positives
                  [Report]) -- false negatives

compareReport clt clg ft fg trs grs =
  let trs' = [r | r <- trs, show (clt r) =~ ft]
      grs' = [r | r <- grs, show (clg r) =~ fg]
      overlaps r1 r2 = file r1 == file r2 &&
        not (line r1 > endLine r2 || line r2 > endLine r1)
      tps = [r | r <- trs', any (overlaps r) grs']
      fps = [r | r <- trs', not $ any (overlaps r) grs']
      fns = [m | m <- grs', not $ any (overlaps m) trs']
  in (tps, fps, fns)


flatten :: Value -> [(String, -- CVE
                      String, -- CWE
                      FilePath, -- file
                      String)] -- function name

flatten (Object o) = flip foldMapWithKey o (\cve (Object r) ->
                                               fromJust (flip parseMaybe r $ \obj -> do
                                                   cwe <- obj .: fromString "cwe"
                                                   Array functions <- obj .: fromString "functions"
                                                   return $ flip fmap (Vector.toList functions) (\(Object f)-> fromJust (flip parseMaybe f $ \obj -> do
                                                                                                                            file <- obj .: fromString "file"
                                                                                                                            name <- obj .: fromString "name"
                                                                                                                            return (toString cve, cwe, file, name)))))

flatten _ = error "Expecting a dictionary."

classifyMagmaReport :: ReportClassifier
classifyMagmaReport r | r.desc == "CWE-476" = CWE476
                      | otherwise = NotRelevant

runClang :: ToolOpts -> ProjectOpts -> IO ()
runClang topts popts = do
  createDirectoryIfMissing True popts.outputDir
  withCurrentDirectory popts.outputDir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
    want ["clang.true.positive.csv", "clang.false.positive.csv", "clang.false.negative.csv"]
    rules topts popts


runClog :: ToolOpts -> ProjectOpts -> IO ()
runClog topts popts = do
  createDirectoryIfMissing True popts.outputDir
  withCurrentDirectory popts.outputDir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
    want ["clog.true.positive.csv", "clog.false.positive.csv", "clog.false.negative.csv"]
    rules topts popts
