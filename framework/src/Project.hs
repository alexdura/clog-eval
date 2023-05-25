{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Project (Project(..), buildProject') where

import Data.Maybe
import Development.Shake
import Clang.CompilationDatabase
import Data.Aeson
import CDB
import Data.Text
import ClangStaticAnalyzer

data Project = Project {
  name :: String,
  path :: FilePath,
  configCmd :: Maybe String,
  buildCmd :: String,
  extraArg :: [String]
  } deriving (Show, Eq)


buildProject' :: Rules ()
buildProject' = do
  want ["run_commands"]

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

  phony "run_commands" $ do
    need ["compile_commands.json"]
    cdb <- liftIO  (decodeFileStrict "compile_commands.json" :: IO (Maybe CompilationDatabase))
    let adjustedCdb = adjustCompileCommands
                      (pack <$> clangAnalyzerPreOpts)
                      (pack <$> clangAnalyzerPostOpts)
                      (fromJust cdb)
        outputCdb = fmap (\c -> c { arguments = Just $ (fromJust c.arguments) ++ (pack <$> ["-o", unpack c.file ++ ".sarif"]) }) adjustedCdb

    runCompileCommands outputCdb


  phony "debug_commands" $ do
    need ["compile_commands.json"]
    cdb <- liftIO  (decodeFileStrict "compile_commands.json" :: IO (Maybe CompilationDatabase))
    let adjustedCdb = adjustCompileCommands
                      (pack <$> clangAnalyzerPreOpts)
                      (pack <$> clangAnalyzerPostOpts)
                      (fromJust cdb)
        outputCdb = fmap (\c -> c { arguments = Just $ (fromJust c.arguments) ++ (pack <$> ["-o", unpack c.file ++ ".sarif"]) }) adjustedCdb

    liftIO $ print $ show outputCdb

runCompileCommands :: CompilationDatabase -> Action ()
runCompileCommands = foldMap (\c -> cmd_ (unpack <$> fromJust c.arguments))
