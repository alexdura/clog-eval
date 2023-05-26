{-# LANGUAGE OverloadedRecordDot #-}

module Juliet (execute, clean) where

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



rules :: FilePath -- ^ The directory
      -> [FilePath] -- ^ Include directories
      -> [String] -- ^ Clang analyses to run
      -> Rules() -- ^ Rules

rules d incs xargs = do
  "compile_commands.json" %> \out -> do
    c_files <- getDirectoryFiles "." ["//*.c"]
    absCFiles <- liftIO $ mapM canonicalizePath c_files
    let cdb = buildCommandObject d incs <$> absCFiles
    liftIO $ encodeFile out cdb

  ["_output" </> "analysis.out", "_output" </> "analysis.time"] &%> \[out, time] -> do
    need ["compile_commands.json"]
    Just cdb <- liftIO (decodeFileStrict "compile_commands.json" :: IO (Maybe CDB.CompilationDatabase))
    let files = CDB.file <$> cdb
    (CmdTime t) <- cmd (FileStdout out)  "clang-tidy" (xargs ++ (unpack <$> files))
    writeFile' time (show t)

  "_output" </> "analysis.csv" %> \out -> do
    need ["_output/analysis.out"]
    lines <- readFileLines "_output/analysis.out"
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


execute :: FilePath -> [FilePath] -> [String] -> IO ()
execute path incs xargs = withCurrentDirectory path $ shake shakeOptions {shakeVerbosity=Verbose} $ do
  want ["_output" </> "analysis.csv"]
  rules path incs xargs


clean :: FilePath -> IO ()
clean path = withCurrentDirectory path $ shake shakeOptions {shakeVerbosity=Verbose} $ do
  want ["clean"]
  rules path [] []
