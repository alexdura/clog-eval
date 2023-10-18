module Program (generate) where

import Development.Shake
import Development.Shake.FilePath
import System.Directory

rules :: Rules ()

rules = do
  "uncontrolled_format_string.mdl" %> \out -> do
    let input = out <.> "in"
    need [input, "taint_propagation.mdl"]
    cmd "cpp" input "-o" out


generate :: FilePath -> IO ()

generate dir = do
  withCurrentDirectory dir $ shake shakeOptions {shakeVerbosity = Verbose} $ do
    want ["uncontrolled_format_string.mdl"]
    rules
