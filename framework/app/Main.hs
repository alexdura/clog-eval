module Main (main) where

import Project

import System.Directory
import Development.Shake

import Control.Monad

projects :: [Project]
projects = [
  Project "zlib" "../projects/zlib" (Just "./configure") "bear make -j `nproc`" []
--  , Project "ffmpeg" "../projects/ffmpeg" (Just "./configure") "bear make -j `nproc`" []
  ]

main :: IO ()
main = forM_ projects $
  \p -> withCurrentDirectory (path p) $ shakeArgs shakeOptions {shakeVerbosity=Verbose} buildProject'
