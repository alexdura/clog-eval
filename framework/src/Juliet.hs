{-# LANGUAGE OverloadedRecordDot #-}

module Juliet (runClang, runClog, clean, JulietOpts(..), defaultJulietOpts) where

import Development.Shake
import Development.Shake.FilePath
import qualified Clang.CompilationDatabase as CDB
import Data.Aeson
import Data.Text
import System.Directory
import Text.CSV
import Text.Regex.TDFA
import Data.Maybe



------------------------------ Report ------------------------------

data Report = Report {
  file :: FilePath,
  line :: Int,
  col :: Int,
  desc :: String,
  kind :: ReportKind
  } deriving (Eq, Show)

data ReportKind = WarningReport
                | ErrorReport
                | OtherReport
                deriving (Eq, Show)

processClangTidyOutput' r [] = [r]
processClangTidyOutput' r (l:ls) = case extractReport l of
                                     Just r' -> (r : processClangTidyOutput' r' ls)
                                     Nothing -> processClangTidyOutput' r { desc = r.desc ++ " " ++ l } ls

processClangTidyOutput :: [String] -> [Report]
processClangTidyOutput [] = []
processClangTidyOutput (l:ls) = case extractReport l of
                                   Just r -> processClangTidyOutput' r ls
                                   Nothing -> processClangTidyOutput ls

extractReport :: String -> Maybe Report
extractReport l = do
  (_, _, _, [f, l, c, k, m]) <- l =~~ "(.+):(.+):(.+): (.+): (.+)" :: Maybe (String, String, String, [String])
  return $ Report f (read l) (read c) m (case k of
                                           "warning" -> WarningReport
                                           "error" -> ErrorReport
                                           _ -> OtherReport)

extractChecker :: Report -> Maybe String
extractChecker (Report _ _ _ desc WarningReport) = do
  (_, _, _, m:_) <- desc =~~ "\\[([^[:space:]]+)\\]" :: Maybe (String, String, String, [String])
  return m
extractChecker _ = Nothing

buildCommandObject :: FilePath -> [FilePath] -> FilePath -> CDB.CommandObject
buildCommandObject d incs f =
  let args = Just $ pack <$> ["clang", f, "-o", f -<.>  exe] ++ (("-I" ++) <$> incs) in
    CDB.CommandObject (pack d) (pack f) Nothing args (Just $ pack $ f -<.> exe)


data JulietOpts = JulietOpts {
  dir :: FilePath,
  clangXargs :: [String],
  clogXargs :: [String],
  includes :: [FilePath],
  clogJar :: FilePath,
  clogProgram :: FilePath
  }

defaultJulietOpts d = JulietOpts d [] [] [] "compiler.jar" ""


rules :: JulietOpts -> Rules()

rules opts = do
  "compile_commands.json" %> \out -> do
    c_files <- getDirectoryFiles "." ["//*.c"]
    absCFiles <- liftIO $ mapM canonicalizePath c_files
    let cdb = buildCommandObject opts.dir opts.includes <$> absCFiles
    liftIO $ encodeFile out cdb

  ["_output" </> "clang.analysis.out", "_output" </> "clang.analysis.time"] &%> \[out, time] -> do
    need ["compile_commands.json"]
    Just cdb <- liftIO (decodeFileStrict "compile_commands.json" :: IO (Maybe CDB.CompilationDatabase))
    let files = CDB.file <$> cdb
    (CmdTime t) <- cmd (FileStdout out)  "clang-tidy" (opts.clangXargs ++ (unpack <$> files))
    writeFile' time (show t)

  ["_output" </> "clog.analysis.csv", "_output" </> "clog.analysis.time"] &%> \[out, time] -> do
    need ["compile_commands.json"]
    Just cdb <- liftIO (decodeFileStrict "compile_commands.json" :: IO (Maybe CDB.CompilationDatabase))
    let files = CDB.file <$> cdb
    (CmdTime t) <- cmd (FileStdout out) "java" "-jar" opts.clogJar "-lang" "c4"
      "-S" (Prelude.foldr1 (\x y -> x ++ ":" ++ y) ((++ ",A") <$> unpack <$> files))
      "-D" "_output"
      opts.clogXargs
      opts.clogProgram
    writeFile' time (show t)

  "_output" </> "clang.analysis.csv" %> \out -> do
    need ["_output/clang.analysis.out"]
    lines <- readFileLines "_output/clang.analysis.out"
    let reports = fmap (\r -> [file r
                              , show $ line r
                              , show $ col r
--                              , desc r
                              , fromJust $ extractChecker r
                                ]) $
                  Prelude.filter (\r -> kind r == WarningReport && isJust (extractChecker r))
                  (processClangTidyOutput lines)
    writeFile' out (printCSV reports)


  phony "clean" $ do
    removeFilesAfter "_output" ["*"]
    removeFilesAfter "." ["compile_commands.json"]


runClang :: JulietOpts -> IO ()
runClang opts = withCurrentDirectory opts.dir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
  want ["_output" </> "clang.analysis.csv"]
  rules opts

runClog opts = withCurrentDirectory opts.dir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
  want ["_output" </> "clog.analysis.csv"]
  rules opts


clean :: JulietOpts -> IO ()
clean opts = withCurrentDirectory opts.dir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
  want ["clean"]
  rules opts
