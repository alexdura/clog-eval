module Main (main) where


import System.Directory
import Development.Shake
import Options.Applicative
import Control.Monad
import System.FilePath
import Data.Aeson

import qualified Project
import qualified Juliet
import qualified Magma

projects :: [Project.Project]
projects = [
  Project.Project "zlib" "../projects/zlib" (Just "./configure") "bear make -j `nproc`" []
--  , Project "ffmpeg" "../projects/ffmpeg" (Just "./configure") "bear make -j `nproc`" []
  ]


data Options = Options {
  mode :: String,
  desc :: FilePath,
  tool :: FilePath
  } deriving (Show)


cliOptions = Options
  <$> strOption (long "mode" <> metavar "MODE" <> help "Mode: 'juliet', 'project', 'magma'" <> showDefault <> value "juliet")
  <*> strOption (long "desc" <> metavar "DESC" <> help "Run description")
  <*> strOption (long "tool" <> metavar "TOOL" <> help "Tool description" <> showDefault <> value "/dev/null")


handleOptions (Options "project"  _ _) = forM_ projects $
                                      \p -> withCurrentDirectory (Project.path p) $ shake shakeOptions {shakeVerbosity=Verbose} Project.buildProject'

handleOptions (Options "juliet" desc _) = do
  jo <- eitherDecodeFileStrict desc :: IO (Either String Juliet.JulietOpts)
  case jo of
    Left m -> putStrLn m
    Right j -> do print j
                  Juliet.runClog j
                  Juliet.runClang j

handleOptions (Options "magma" desc tool) = do
  print desc
  print tool
  po <- eitherDecodeFileStrict desc :: IO (Either String Magma.ProjectOpts)
  case po of
    Left m -> putStrLn m
    Right p -> do print p
                  to <- eitherDecodeFileStrict tool :: IO (Either String Magma.ToolOpts)
                  case to of
                    Left m -> putStrLn m
                    Right t -> do print t
                                  Magma.runClog t p
                                  Magma.runClang t p

main :: IO ()
main = let opts = info (cliOptions <**> helper)
                  (fullDesc <> progDesc "Generate appropriate compile commands files")
       in handleOptions =<< execParser opts
