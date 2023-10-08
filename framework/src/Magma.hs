{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Magma (runClang, runClog) where

import GHC.Generics
import Development.Shake
import Development.Shake.FilePath

data MagmaOpts = MagmaOpts {
  dir :: FilePath,
  clangXargs :: [String],
  clogXargs :: [String],
  includes :: [FilePath],
  clogJar :: FilePath,
  clogProgramPath :: FilePath,
  clangFilter :: String,
  clogFilter :: String,
  outputDir :: FilePath
} deriving (Show, Generic)

runClang = undefined
runClog = undefined

generateCompileCommands :: MagmaOpts -> FilePath -> Action ()

generateCompileCommands opts outf = undefined
