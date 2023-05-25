module Juliet where

import Development.Shake
import Development.Shake.FilePath
import Clang.CompilationDatabase
import Data.Aeson
import ClangStaticAnalyzer
import Data.Text
import System.Directory

buildCommandObject :: FilePath -> [FilePath] -> FilePath -> CommandObject
buildCommandObject d incs f =
  let args = Just $ pack <$> ["clang", f, "-o", f -<.>  exe] ++ (("-I" ++) <$> incs) in
    CommandObject (pack d) (pack f) Nothing args (Just $ pack $ f -<.> exe)

rules :: FilePath -> [FilePath] -> Rules()
rules d incs = do
  want ["compile_commands.json"]

  "compile_commands.json" %> \out -> do
    c_files <- getDirectoryFiles "." ["//*.c"]
    absCFiles <- liftIO $ mapM canonicalizePath c_files
    let cdb = buildCommandObject d incs <$> absCFiles
    liftIO $ encodeFile out cdb

generateCompileCommands :: FilePath -> [FilePath] -> IO ()
generateCompileCommands path incs = withCurrentDirectory path $ shake shakeOptions {shakeVerbosity=Verbose} (rules path incs)
