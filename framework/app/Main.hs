module Main (main) where


import System.Directory
import Development.Shake
import Options.Applicative
import Control.Monad
import System.FilePath

import qualified Project
import qualified Juliet

projects :: [Project.Project]
projects = [
  Project.Project "zlib" "../projects/zlib" (Just "./configure") "bear make -j `nproc`" []
--  , Project "ffmpeg" "../projects/ffmpeg" (Just "./configure") "bear make -j `nproc`" []
  ]


data Options = Options {
  srcDir :: FilePath,
  julietDir :: FilePath,
  mode :: String,
  jar :: FilePath,
  clogProgram :: FilePath,
  julietFilter :: String
  }


cliOptions = Options
  <$> strOption (long "srcdir" <> metavar "SRC_DIR" <> help "Source directory")
  <*> strOption (long "juliet-dir" <> metavar "JULIET_DIR" <> help "Juliet subdirectory")
  <*> option auto (long "mode" <> metavar "MODE" <> help "Mode: 'juliet' or 'project'" <> showDefault <> value "juliet")
  <*> strOption (long "jar" <> metavar "JAR" <> help "Clog jar" <> value "compiler.jar")
  <*> strOption (long "clog-program" <> metavar "CLOG_PROGRAM" <> help "Clog program path")
  <*> strOption (long "juliet-filter" <> metavar "JULIET_FILTER" <> help "Juliet filter (regex)")

handleOptions (Options _ _ "project"  _ _ _) = forM_ projects $
                                      \p -> withCurrentDirectory (Project.path p) $ shake shakeOptions {shakeVerbosity=Verbose} Project.buildProject'

handleOptions (Options d jd "juliet" jar clogp jf) = do
  absIncs <- mapM canonicalizePath [d </> "testcasesupport"]
  absD <- canonicalizePath $ d </> jd
  absClogP <- canonicalizePath clogp
  manifestP <- canonicalizePath $ d </> "manifest.xml"

  Juliet.clean $ Juliet.defaultJulietOpts absD

  let clangOpts = (Juliet.defaultJulietOpts absD) {
    Juliet.includes = absIncs,
    Juliet.clangXargs = ["--checks=clang-analyzer-core.uninitialized.Assign,clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.uninitialized.Branch"] }
  Juliet.runClang $ clangOpts

  Juliet.runClog $ (Juliet.defaultJulietOpts absD) {
    Juliet.includes = absIncs,
    Juliet.clogXargs = [],
    Juliet.clogJar = jar,
    Juliet.clogProgramPath = absClogP,
    Juliet.manifestFilter = jf,
    Juliet.manifest = manifestP
    }


main :: IO ()
main = let opts = info (cliOptions <**> helper)
                  (fullDesc <> progDesc "Generate appropriate compile commands files")
       in handleOptions =<< execParser opts
