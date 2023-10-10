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
import qualified Data.Vector as Vector
import qualified Clang.CompilationDatabase as CDB

import Report
import ClangTidy
import Clog


data ProjectOpts = ProjectOpts {
  dir :: FilePath,
  groundTruth :: FilePath,
  includes :: [FilePath],
  clangFilter :: String,
  clogFilter :: String,
  outputDir :: FilePath
} deriving (Show, Generic)


data ToolOpts = ToolOpts {
  clangXargs :: [String],
  clogXargs :: [String],
  clogJar :: FilePath,
  clogProgramPath :: FilePath,
  clogUtilPath :: FilePath
  } deriving (Show, Generic)



instance FromJSON ProjectOpts
instance FromJSON ToolOpts
instance ToJSON ProjectOpts
instance ToJSON ToolOpts


runClang :: ToolOpts -> ProjectOpts -> IO ()

runClog = undefined

rules :: ToolOpts -> ProjectOpts -> Rules()
rules topts popts = do
  let compile_commands = popts.dir </> "compile_commands.json"
      patched_compile_commands = "compile_commands.json"
      sources_csv = "srcs.csv"
      includes = ("-I" ++) <$> popts.includes

  sources_csv %> \out -> do
    need [compile_commands]
    Just cdb <- liftIO (decodeFileStrict compile_commands :: IO (Maybe CDB.CompilationDatabase))
    let files = (\e -> [unpack (CDB.directory e) </> unpack (CDB.file e), "A"]) <$> cdb
    liftIO $ writeFile' out (printCSV files)

  patched_compile_commands %> \out -> do
    need [compile_commands]
    Just cdb <- liftIO (decodeFileStrict compile_commands :: IO (Maybe CDB.CompilationDatabase))
    let patched_cdb = (\e -> e { CDB.arguments = CDB.arguments e >>= \args -> return $ args ++ (pack <$> includes) }) <$> cdb
    liftIO $ encodeFile out patched_cdb

  ["clang.analysis.out", "clang.analysis.time"] &%> \[out, time] -> do
    need [patched_compile_commands]
    Just cdb <- liftIO (decodeFileStrict patched_compile_commands :: IO (Maybe CDB.CompilationDatabase))
    let files = (\e -> unpack (CDB.directory e) </> unpack (CDB.file e)) <$> cdb
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

  "func.range.csv" %> \out -> do
    need [popts.groundTruth, sources_csv, patched_compile_commands]
    Just jsonGT <- liftIO (decodeFileStrict popts.groundTruth :: IO (Maybe Value))
    let tabularGT = fmap (\(t0, t1, t2, t3) -> [t0, t1, t2, t3]) $ flatten jsonGT
    writeFile' "ground.truth.csv" (printCSV tabularGT)
    cmd "java" "-jar" topts.clogJar topts.clogUtilPath
      "-lang" "c4"
      "-S" sources_csv
      ["-Xclang=-p ."]
      "-D" "."
      "-F" "."
      "-debug"


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

runClang topts popts = do
  createDirectoryIfMissing True popts.outputDir
  withCurrentDirectory popts.outputDir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
    want ["srcs.csv", "clang.analysis.csv", "func.range.csv"]
    rules topts popts
