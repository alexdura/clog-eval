{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}


module Juliet (runClang, runClog, clean, JulietOpts(..), defaultJulietOpts, extractReportsXML) where

import Data.Aeson
import Data.Maybe
import Data.Text ( pack, unpack )
import Development.Shake
import Development.Shake.FilePath
import GHC.Generics
import System.Directory
import Text.CSV
import Text.Printf
import Text.Regex.TDFA
import Text.XML.HXT.Core
import qualified Clang.CompilationDatabase as CDB

import Report
import ClangTidy
import Clog

------------------------------ Report ------------------------------
classifyJulietReport :: Report -> ReportClass
classifyJulietReport (Report _ _ _ "CWE-457: Use of Uninitialized Variable" _) = Juliet_CWE457
classifyJulietReport (Report _ _ _ "CWE-416: Use After Free" _) = Juliet_CWE416
classifyJulietReport _ = NotRelevant


buildCommandObject :: FilePath -> [FilePath] -> FilePath -> CDB.CommandObject
buildCommandObject d incs f =
  let args = Just $ pack <$> ["clang", f, "-o", f -<.>  exe] ++ (("-I" ++) <$> incs) in
    CDB.CommandObject (pack d) (pack f) Nothing args (Just $ pack $ f -<.> exe)


data JulietOpts = JulietOpts {
  dir :: FilePath,
  clangXargs :: [String],
  clogXargs :: [String],
  includes :: [FilePath],
  clogJar :: FilePath,
  clogProgramPath :: FilePath,
  manifest :: FilePath,
  manifestFilter :: String,
  outputDir :: FilePath
  }
  deriving (Show, Generic)

instance FromJSON JulietOpts
instance ToJSON JulietOpts

defaultJulietOpts d = JulietOpts d [] [] [] "compiler.jar" "." "." "." "_output"

rules :: JulietOpts -> Rules()
rules opts = do
  let compile_commands = "compile_commands.json"
  -- build the compile commands file needed by the clang static analyzer
  compile_commands %> \out -> do
    c_files <- getDirectoryFiles opts.dir ["//*.c"]
    liftIO $ putStrLn opts.dir
    let absCFiles = (opts.dir </>) <$> c_files
    let cdb = buildCommandObject opts.dir opts.includes <$> absCFiles
    liftIO $ encodeFile out cdb

  -- run the Clang static analyzer through clang-tidy
  ["clang.analysis.out", "clang.analysis.time"] &%> \[out, time] -> do
    need [compile_commands]
    Just cdb <- liftIO (decodeFileStrict compile_commands :: IO (Maybe CDB.CompilationDatabase))
    let files = CDB.file <$> cdb
    (CmdTime t) <- cmd (FileStdout out) (FileStderr "clang.analysis.err")
      "clang-tidy"
      "-p" (opts.dir </> opts.outputDir)
      (opts.clangXargs ++ (unpack <$> files))
    writeFile' time (show t)

  -- run the Clog analysis
  ["clog.analysis.csv", "clog.analysis.time"] &%> \[out, time] -> do
    need [compile_commands,
          opts.clogProgramPath]
    Just cdb <- liftIO (decodeFileStrict compile_commands :: IO (Maybe CDB.CompilationDatabase))
    let files = CDB.file <$> cdb
    (CmdTime t) <- cmd (FileStdout "clog.analysis.out") (FileStderr "clog.analysis.err")
      "java" "-jar" opts.clogJar "-lang" "c4"
      "-S" (Prelude.foldr1 (\x y -> x ++ ":" ++ y) ((++ ",A") . unpack <$> files))
      ["-Xclang=-p ."]
      "-D" "."
      opts.clogXargs
      opts.clogProgramPath
    writeFile' time (show t)

  -- clasify the Clog reports between true positive and false positives
  ["clog.true.positive.csv",
   "clog.false.positive.csv",
   "clog.false.negative.csv"] &%> \[tpCsv,fpCsv, fnCsv] -> do
    need ["clog.analysis.csv"]
    r <- liftIO $ extractReportsCSV "clog.analysis.csv" -- extract the report produced by clog
    g <- liftIO $ extractReportsXML opts.manifest -- extract the ground truth
    let fg = filter (\r -> r.file =~ opts.manifestFilter) g
        r' = map (\rep -> rep { file = takeFileName rep.file }) r
        tp = reportIntersect classifyClogReport classifyJulietReport r' fg
        fp = reportDiff classifyClogReport classifyJulietReport r' fg
        fn = reportDiff classifyJulietReport classifyClogReport fg r'
        toCsvLine rl = [file rl, show $ line rl , desc rl]
    writeFile' tpCsv (printCSV $ map toCsvLine tp)
    writeFile' fpCsv (printCSV $ map toCsvLine fp)
    writeFile' fnCsv (printCSV $ map toCsvLine fn)

  -- post-process the output of the Clang static analyzer
  "clang.analysis.csv" %> \out -> do
    need ["clang.analysis.out"]
    lines <- readFileLines $ "clang.analysis.out"
    let reports = fmap (\r -> [file r
                              , show $ line r
                              , show $ col r
                              , desc r
                                ]) $
                  Prelude.filter (\r -> kind r == WarningReport && isJust (extractChecker r))
                  (processClangTidyOutput lines)
    writeFile' out (printCSV reports)

  phony "clang-clog-compare" $ do
    need ["clog.analysis.csv", "clang.analysis.csv"]
    clogReport <- liftIO $ extractReportsCSV $ "clog.analysis.csv"
    clangReport <- liftIO $ extractReportsCSV $ "clang.analysis.csv"
    groundReport <- liftIO $ extractReportsXML opts.manifest
    let normFileName r = r { file = takeFileName r.file }
        clangReport' = normFileName <$> clangReport
        clogReport' = normFileName <$> clogReport
        groundReport' = filter (\r -> file r =~ opts.manifestFilter) groundReport
        groundAndClang = reportIntersect classifyJulietReport classifyClangReport groundReport' clangReport'
        clangAndClog = reportIntersect classifyClangReport classifyClogReport clangReport' clogReport'
        groundAndClangNotClog = reportDiff classifyJulietReport classifyClogReport groundAndClang clogReport'
        clangAndClogNotGround = reportDiff classifyClangReport classifyJulietReport clangAndClog groundReport'
        clogNotClang = reportDiff classifyClogReport classifyClangReport clogReport' clangReport'
        clogNotGround = reportDiff classifyClogReport classifyJulietReport clogReport' groundReport'
        clogNotClangNotGround = reportIntersect classifyClogReport classifyClogReport clogNotClang clogNotGround
        toCsvLine r = [file r, show $ line r, desc r]
    liftIO $ do
      putStrLn "clang/clog compare where clang is the ground truth"
      putStrLn $ printf "True positives reported by clang and not clog %d" (length groundAndClangNotClog)
      putStrLn $ printf "False positives where both clang and clog agree %d" (length clangAndClogNotGround)
      putStrLn $ printf "False positives reported by clog and not clang %d" (length clogNotClangNotGround)
    writeFile' ("ground-and-clang-not-clog.csv") (printCSV $ map toCsvLine groundAndClangNotClog)
    writeFile' ("clang-and-clog-not-ground.csv") (printCSV $ map toCsvLine clangAndClogNotGround)
    writeFile' ("clog-not-clang-not-ground.csv") (printCSV $ map toCsvLine clogNotClangNotGround)


  -- phony "stats-clog"
  printStats opts classifyClogReport "clog.analysis.csv" "stats-clog"

  -- phony "stats-clang"
  printStats opts classifyClangReport "clang.analysis.csv" "stats-clang"

  phony "clean" $ do
    removeFilesAfter (opts.dir </> opts.outputDir) ["*"]

printStats :: JulietOpts -> (Report -> ReportClass) -> String -> String -> Rules ()
printStats opts classifier inReport name =
  phony name $ do
    need [inReport]
    r <- liftIO $ extractReportsCSV $ inReport -- extract the report produced by clog
    g <- liftIO $ extractReportsXML opts.manifest -- extract the ground truth

    let fg = filter (\r -> r.file =~ opts.manifestFilter) g
        r' = map (\rep -> rep { file = takeFileName rep.file }) r
        tp = reportIntersect classifier classifyJulietReport r' fg
        fp = reportDiff classifier classifyJulietReport r' fg
        fn = reportDiff classifyJulietReport classifier fg r'
    liftIO $ do
      putStrLn name
      putStrLn ((printf "True positives %d/%d" (length tp) (length fg)) :: String)
      putStrLn ((printf "False positives %d/%d" (length fp) (length fg)) :: String)
      putStrLn ((printf "False negatives %d/%d" (length fn) (length fg)) :: String)


runClang :: JulietOpts -> IO ()
runClang opts = do
  let buildDir = opts.dir </> opts.outputDir
  createDirectoryIfMissing True buildDir
  withCurrentDirectory buildDir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
    want ["clang.analysis.out"]
    rules opts

runClog  :: JulietOpts -> IO ()
runClog opts = do
  let buildDir = opts.dir </> opts.outputDir
  createDirectoryIfMissing True buildDir
  withCurrentDirectory buildDir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
    want $ "clang-clog-compare" : "stats-clog" : "stats-clang" : ["clog.true.positive.csv", "clog.false.positive.csv", "clog.analysis.csv"]
    rules opts

extractReportXML :: ArrowXml a => a XmlTree (Report)
extractReportXML = deep (isElem >>> hasName "file") >>>
  proc x -> do
    file <- getAttrValue "path" -< x
    flaw <- deep (isElem >>> hasName "flaw") -< x
    flawLine <- getAttrValue "line" -< flaw
    flawName <- getAttrValue "name" -< flaw
    returnA -< Report file ((read flawLine)::Int) 0 flawName OtherReport

extractReportsXML :: FilePath -> IO [Report]
extractReportsXML f = runX (readDocument [withValidate no] f >>> extractReportXML)

extractReportsCSV :: FilePath -> IO [Report]
extractReportsCSV f = do
  Right csv <- parseCSVFromFile f
  return $ fmap (\[f, l, c, err] -> Report f ((read l)::Int) ((read c)::Int) err OtherReport) $ filter (/= [""]) csv

clean :: JulietOpts -> IO ()
clean opts = do
  let buildDir = opts.dir </> opts.outputDir
  createDirectoryIfMissing True buildDir
  withCurrentDirectory buildDir$ shake shakeOptions {shakeVerbosity=Verbose} $ do
    want ["clean"]
    rules opts


reportEq :: Report -> Report -> Bool
reportEq l r = file l == file r && line l == line r

compareResultsFileLineOnly :: [Report] -- ground truth report
                           -> [Report] -- tool reports
                           -> ([Report], -- true positives
                               [Report], -- false positives
                               [Report]) -- false negatives

-- compareResultsFileLineOnly g t = (intersectBy reportEq g t,
--                                   deleteFirstsBy reportEq t g,
--                                   deleteFirstsBy reportEq g t)
compareResultsFileLineOnly = undefined
