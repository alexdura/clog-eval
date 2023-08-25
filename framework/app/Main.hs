module Main (main) where


import System.Directory
import Development.Shake
import Options.Applicative
import Control.Monad
import System.FilePath
import Data.Aeson

import qualified Project
import qualified Juliet

projects :: [Project.Project]
projects = [
  Project.Project "zlib" "../projects/zlib" (Just "./configure") "bear make -j `nproc`" []
--  , Project "ffmpeg" "../projects/ffmpeg" (Just "./configure") "bear make -j `nproc`" []
  ]


data Options = Options {
  mode :: String,
  desc :: FilePath
  } deriving (Show)


cliOptions = Options
  <$> strOption (long "mode" <> metavar "MODE" <> help "Mode: 'juliet', 'project'" <> showDefault <> value "juliet")
  <*> strOption (long "desc" <> metavar "DESC" <> help "Run description")



handleOptions (Options "project"  _) = forM_ projects $
                                      \p -> withCurrentDirectory (Project.path p) $ shake shakeOptions {shakeVerbosity=Verbose} Project.buildProject'

handleOptions (Options "juliet" desc) = do
  Just jo <- decodeFileStrict desc :: IO (Maybe Juliet.JulietOpts)
  print jo
  Juliet.runClog jo


main :: IO ()
main = let opts = info (cliOptions <**> helper)
                  (fullDesc <> progDesc "Generate appropriate compile commands files")
       in handleOptions =<< execParser opts
