{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Project (Project(..), buildProject, buildProject') where

import System.Process
import System.Exit
import Data.Maybe
import Development.Shake
import Clang.CompilationDatabase
import Data.Aeson
import CDB
import Data.Text


data Project = Project {
  name :: String,
  path :: FilePath,
  configCmd :: Maybe String,
  buildCmd :: String,
  extraArg :: [String]
  } deriving (Show, Eq)


-- procDesc = CreateProcess (ShellCommand $ buildCmd p) (Just $ path p) Nothing Inherit Inherit Inherit False

runConfig :: Project -> IO ExitCode
runConfig p =
  let configDesc = (shell (fromJust $ configCmd p)) { cwd = Just $ path p } in do
    (_, _, _,  hProc) <- createProcess configDesc
    waitForProcess hProc

runBuild :: Project -> IO ExitCode

runBuild p =
  let procDesc = (shell (buildCmd p)) { cwd = Just $ path p } in do
    (_, _, _,  hProc) <- createProcess procDesc
    waitForProcess hProc


buildProject :: Project -> IO ExitCode
buildProject p = do
  if isJust (configCmd p) then
    do
      ExitSuccess <- runConfig p
      runBuild p
    else runBuild p

clangAnalyzerPreOpts :: [String]
clangAnalyzerPreOpts = ["--analyze"]
clangAnalyzerPostOpts :: [String]
clangAnalyzerPostOpts = ["-Xanalyzer", "-analyzer-output=sarif"] ++ clangAnalysisOpts
clangAnalysisOpts :: [String]
clangAnalysisOpts =  ["-Xclang", "-analyzer-checker=core.uninitialized"] -- Check for uninitialized values used as array subscripts
                      -- "-analyzer-checker", "core.uninitialized.Assign",     -- Check for assigning uninitialized values
                      -- "-analyzer-checker", "core.uninitialized.Branch", --     Check for uninitialized values used as branch conditions
                      -- "-analyzer-checker", "core.uninitialized.CapturedBlockVariable", --  Check for blocks that capture uninitialized values
                      -- "-analyzer-checker", "core.uninitialized.UndefReturn"]

buildProject' :: Rules ()
buildProject' = do
  want ["clean", "run_commands"]

  phony "clean" $ do
    putInfo "Cleaning files"
    cmd_ "make clean"
    removeFilesAfter "clang_static_analyzer" ["//*"]

  "configure.log" %> \_ ->  do
    cmd_ "./configure"

  "compile_commands.json" %> \_ -> do
    need ["configure.log", "clean"]
    cmd_ "bear" "make" "-j4"

  "compile_commands.json.pp" %> \out -> do
    need ["compile_commands.json"]
    cmd_ (FileStdout out) Shell "jq" "'" "[.[]|.[\"command\"]=.arguments|del(.arguments)]" "'" "compile_commands.json"

  "clang_static_analyzer" %> \out -> do
    need ["compile_commands.json.pp"]
    cmd_ "analyze-build"
      "--cdb" "compile_commands.json.pp"
      "-o" out
      "-vvvv"
      "-sarif"
      "-analyzer-config" "stable-report-filename=true"
      "-enable-checker" "core.uninitialized.ArraySubscript" -- Check for uninitialized values used as array subscripts
      "-enable-checker" "core.uninitialized.Assign"     -- Check for assigning uninitialized values
      "-enable-checker" "core.uninitialized.Branch" --     Check for uninitialized values used as branch conditions
      "-enable-checker" "core.uninitialized.CapturedBlockVariable" --  Check for blocks that capture uninitialized values
      "-enable-checker" "core.uninitialized.UndefReturn"

  phony "run_commands" $ do
    need ["compile_commands.json"]
    -- liftIO $ do
    --   j <- decodeFileStrict "compile_commands.json" :: IO (Maybe CompilationDatabase)
    --   case j of
    --     Just cdb -> print $ show $ adjustCompileCommands
    --                 (pack <$> ["-fsyntax-only"])
    --                 (pack <$> ["--analyze", "-Xanalyzer", "-analyzer-output=sarif", "-o", "clang_static_analyzer"])
    --                  cdb
    --     _ -> print "Error parsing compilation database"
    cdb <- liftIO  (decodeFileStrict "compile_commands.json" :: IO (Maybe CompilationDatabase))
    let adjustedCdb = adjustCompileCommands
                      (pack <$> clangAnalyzerPreOpts)
                      (pack <$> clangAnalyzerPostOpts)
                      (fromJust cdb)
        outputCdb = fmap (\c -> c { arguments = Just $ (fromJust c.arguments) ++ (pack <$> ["-o", unpack c.file ++ ".sarif"]) }) adjustedCdb

    runCompileCommands outputCdb


  phony "debug_commands" $ do
    need ["compile_commands.json"]
    -- liftIO $ do
    --   j <- decodeFileStrict "compile_commands.json" :: IO (Maybe CompilationDatabase)
    --   case j of
    --     Just cdb -> print $ show $ adjustCompileCommands
    --                 (pack <$> ["-fsyntax-only"])
    --                 (pack <$> ["--analyze", "-Xanalyzer", "-analyzer-output=sarif", "-o", "clang_static_analyzer"])
    --                  cdb
    --     _ -> print "Error parsing compilation database"
    cdb <- liftIO  (decodeFileStrict "compile_commands.json" :: IO (Maybe CompilationDatabase))
    let adjustedCdb = adjustCompileCommands
                      (pack <$> clangAnalyzerPreOpts)
                      (pack <$> clangAnalyzerPostOpts)
                      (fromJust cdb)
        outputCdb = fmap (\c -> c { arguments = Just $ (fromJust c.arguments) ++ (pack <$> ["-o", unpack c.file ++ ".sarif"]) }) adjustedCdb

    liftIO $ print $ show outputCdb

runCompileCommands :: CompilationDatabase -> Action ()
runCompileCommands = foldMap (\c -> cmd_ (unpack <$> fromJust c.arguments))
