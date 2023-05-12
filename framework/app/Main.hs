module Main (main) where

import Project

import System.Exit


p1 = Project "zlib" "../projects/zlib" (Just "./configure") "make" []

main :: IO ()
main = do
  exitCode <- buildProject p1
  case exitCode of
    ExitSuccess -> print "Build succesful"
    ExitFailure _ -> print "Exit failure"
