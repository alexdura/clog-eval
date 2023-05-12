module Project (Project(..), buildProject) where

import System.IO
import System.Process
import System.Exit
import Data.Maybe

data Project = Project {
  name :: String,
  path :: FilePath,
  configCmd :: Maybe String,
  buildCmd :: String,
  extraArg :: [String]
  } deriving (Show, Eq)


-- procDesc = CreateProcess (ShellCommand $ buildCmd p) (Just $ path p) Nothing Inherit Inherit Inherit False

runConfig p =
  let configDesc = (shell (fromJust $ configCmd p)) { cwd = Just $ path p } in do
    (hStdin, hStdout, hStderr,  hProc) <- createProcess configDesc
    waitForProcess hProc


runBuild p =
  let procDesc = (shell (buildCmd p)) { cwd = Just $ path p } in do
    (hStdin, hStdout, hStderr,  hProc) <- createProcess procDesc
    waitForProcess hProc


buildProject :: Project -> IO (ExitCode)
buildProject p = do
  if isJust (configCmd p) then
    do
    runConfig p
    runBuild p
    else do
    runBuild p
