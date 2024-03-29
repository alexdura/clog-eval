{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}


module Juliet (runClang, runClog, clean, JulietOpts(..), extractReportsXML) where

import Data.Aeson
import Data.Maybe
import Data.Text ( pack, unpack )
import Data.List ( intercalate )
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
classifyJulietReport (Report _ _ _ _ _ "CWE-457: Use of Uninitialized Variable" _) = CWE457
classifyJulietReport (Report _ _ _ _ _ "CWE-416: Use After Free" _) = CWE416
classifyJulietReport (Report _ _ _ _ _ "CWE-078: Improper Neutralization of Special Elements used in an OS Command ('OS Command Injection')" _) = CWE78
classifyJulietReport (Report _ _ _ _ _ "CWE-476: NULL Pointer Dereference" _) = CWE476
classifyJulietReport (Report _ _ _ _ _ "CWE-134: Uncontrolled Format String" _) = CWE134
classifyJulietReport (Report _ _ _ _ _ "CWE-122: Heap-based Buffer Overflow" _) = CWE122
classifyJulietReport (Report _ _ _ _ _ "CWE-121: Stack-based Buffer Overflow" _) = CWE121
classifyJulietReport (Report _ _ _ _ _ "CWE-126: Buffer Over-read" _) = CWE126
classifyJulietReport (Report _ _ _ _ _ "CWE-127: Buffer Under-read" _) = CWE127
classifyJulietReport (Report _ _ _ _ _ "CWE-124: Buffer Underwrite ('Buffer Underflow')" _) = CWE124
classifyJulietReport _ = NotRelevant


buildCommandObject :: FilePath -> [FilePath] -> FilePath -> CDB.CommandObject
buildCommandObject d incs f =
  let args = ["clang", f, "-o", f -<.>  exe] ++ (("-I" ++) <$> incs) in
    CDB.CommandObject (pack d) (pack f) (Just $ pack $ unwords args) (Just $ pack <$> args) (Just $ pack $ f -<.> exe)


data JulietOpts = JulietOpts {
  dir :: FilePath,
  clangXargs :: [String],
  clogXargs :: [String],
  includes :: [FilePath],
  clogJar :: FilePath,
  clogProgramPath :: FilePath,
  manifest :: FilePath,
  manifestFilter :: String,
  clangFilter :: String,
  clogFilter :: String,
  fileFilter :: String,
  fileExcludeFilter :: Maybe String,
  outputDir :: FilePath
  }
  deriving (Show, Generic)

instance FromJSON JulietOpts
instance ToJSON JulietOpts


acceptFile :: JulietOpts -> FilePath -> Bool
acceptFile opts f = (f =~ opts.fileFilter) && (isNothing opts.fileExcludeFilter || not (f =~ fromJust opts.fileExcludeFilter))

rules :: JulietOpts -> Rules()
rules opts = do
  let compile_commands = "compile_commands.json"
      sources_csv = "srcs.csv"
  -- build the compile commands file needed by the clang static analyzer
  compile_commands %> \out -> do
    c_files <- getDirectoryFiles opts.dir ["//*.c"]
    liftIO $ putStrLn opts.dir
    let absCFiles = (opts.dir </>) <$> filter (acceptFile opts) c_files
    let cdb = buildCommandObject opts.dir opts.includes <$> absCFiles
    liftIO $ encodeFile out cdb

  sources_csv %> \out -> do
    need [compile_commands]
    Just cdb <- liftIO (decodeFileStrict compile_commands :: IO (Maybe CDB.CompilationDatabase))
    let files = CDB.file <$> cdb
    liftIO $ writeFile' out (printCSV $ map (\f -> [unpack f, "A"]) files)

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
    need [sources_csv,
          compile_commands,
          opts.clogProgramPath]
    (CmdTime t) <- cmd (FileStdout "clog.analysis.out") (FileStderr "clog.analysis.err")
      "java" "-jar" opts.clogJar "-lang" "c4"
      "-S" sources_csv
      ["-Xclang=-p ."]
      "-D" "."
      opts.clogXargs
      opts.clogProgramPath
    writeFile' time (show t)

  -- clasify the Clog reports between true positive and false positives
  ["clog.true.positive.csv",
   "clog.false.positive.csv",
   "clog.false.negative.csv",
   "ground.csv"] &%> \[tpCsv,fpCsv, fnCsv, gCsv] -> do
    need ["clog.analysis.csv"]
    clogReport <- liftIO $ extractReportsCSV "clog.analysis.csv" -- extract the report produced by clog
    groundReport <- liftIO $ extractReportsXML opts.manifest -- extract the ground truth
    dumpStats opts.fileFilter opts.fileExcludeFilter opts.manifestFilter opts.clogFilter classifyClogReport groundReport clogReport
      tpCsv fpCsv fnCsv (Just gCsv)

  -- clasify the Clang reports between true positive and false positives
  ["clang.true.positive.csv",
   "clang.false.positive.csv",
   "clang.false.negative.csv"] &%> \[tpCsv,fpCsv, fnCsv] -> do
    need ["clang.analysis.csv"]
    clangReport <- liftIO $ extractReportsCSV "clang.analysis.csv" -- extract the report produced by clang
    groundReport <- liftIO $ extractReportsXML opts.manifest -- extract the ground truth
    dumpStats opts.fileFilter opts.fileExcludeFilter opts.manifestFilter opts.clangFilter classifyClangReport groundReport clangReport
      tpCsv fpCsv fnCsv Nothing


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


  phony "stats-clog" $ do
    need ["clog.analysis.csv"]
    clogReport <- liftIO $ extractReportsCSV $ "clog.analysis.csv"
    groundReport <- liftIO $ extractReportsXML opts.manifest
    liftIO $ do
      putStrLn "Clog precission/recall stats"
      printStats opts.fileFilter opts.fileExcludeFilter opts.manifestFilter opts.clogFilter classifyClogReport groundReport clogReport

  phony "stats-clang" $ do
    need ["clang.analysis.csv"]
    clangReport <- liftIO $ extractReportsCSV "clang.analysis.csv"
    groundReport <- liftIO $ extractReportsXML opts.manifest
    liftIO $ do
      putStrLn "Clang precission/recall stats"
      printStats opts.fileFilter opts.fileExcludeFilter opts.manifestFilter opts.clangFilter classifyClangReport groundReport clangReport


  phony "clean" $ do
    removeFilesAfter (opts.dir </> opts.outputDir) ["*"]

printStats :: String -- file filter
           -> Maybe String -- file exclude filter
           -> String -- ground truth filter (manifest)
           -> String -- report filter
           -> (Report -> ReportClass) -- report classifier
           -> [Report] -- ground reports
           -> [Report] -- tool reports
           -> IO ()

printStats fileFilter fileExcludeFilter manifestFilter reportFilter classifier  groundReport toolReport = do
  let relevantG = filter (\rep -> rep.file =~ fileFilter && (isNothing fileExcludeFilter || not (rep.file =~ fromJust fileExcludeFilter))
                           && show (classifyJulietReport rep) =~ manifestFilter) groundReport
      relevantR = filter (\rep -> rep.file =~ fileFilter && (isNothing fileExcludeFilter || not (rep.file =~ fromJust fileExcludeFilter))
                           && show (classifier rep) =~ reportFilter) $
          map (\rep -> rep { file = takeFileName rep.file }) toolReport
      tp = reportIntersect classifyJulietReport classifier relevantG relevantR
      fp = reportDiff classifier classifyJulietReport relevantR relevantG
      fn = reportDiff classifyJulietReport classifier relevantG relevantR
  liftIO $ do
    putStrLn ((printf "True positives %d/%d" (length tp) (length relevantG)) :: String)
    putStrLn ((printf "False positives %d/%d" (length fp) (length relevantG)) :: String)
    putStrLn ((printf "False negatives %d/%d" (length fn) (length relevantG)) :: String)

dumpStats :: String -- file filter
           -> Maybe String -- file exclude filter
           -> String -- ground truth filter (manifest)
           -> String -- report filter
           -> (Report -> ReportClass) -- report classifier
           -> [Report] -- ground reports
           -> [Report] -- tool reports
           -> FilePath -- true positive
           -> FilePath -- false positive
           -> FilePath -- flase negative
           -> Maybe FilePath -- ground
           -> Action ()
dumpStats fileFilter fileExcludeFilter manifestFilter reportFilter classifier  groundReport toolReport tpCsv fpCsv fnCsv groundCsv = do
  let relevantG = filter (\rep -> rep.file =~ fileFilter && (isNothing fileExcludeFilter || not (rep.file =~ fromJust fileExcludeFilter))
                           && show (classifyJulietReport rep) =~ manifestFilter) groundReport
      relevantR = filter (\rep -> rep.file =~ fileFilter && (isNothing fileExcludeFilter || not (rep.file =~ fromJust fileExcludeFilter))
                           && show (classifier rep) =~ reportFilter) $
          map (\rep -> rep { file = takeFileName rep.file }) toolReport
      tp = reportIntersect classifyJulietReport classifier relevantG relevantR
      fp = reportDiff classifier classifyJulietReport relevantR relevantG
      fn = reportDiff classifyJulietReport classifier relevantG relevantR
      toCsvLine rl = [file rl, show $ line rl , desc rl]
  liftIO $ do
    writeFile' tpCsv (printCSV $ map toCsvLine tp)
    writeFile' fpCsv (printCSV $ map toCsvLine fp)
    writeFile' fnCsv (printCSV $ map toCsvLine fn)
    if (isJust groundCsv) then writeFile' (fromJust groundCsv) (printCSV $ map toCsvLine relevantG)
      else pure ()


runClang :: JulietOpts -> IO ()
runClang opts = do
  let buildDir = opts.dir </> opts.outputDir
  createDirectoryIfMissing True buildDir
  withCurrentDirectory buildDir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
    want ["clang.analysis.out", "clang.true.positive.csv", "clang.false.positive.csv", "clang.analysis.csv"]
    rules opts

runClog  :: JulietOpts -> IO ()
runClog opts = do
  let buildDir = opts.dir </> opts.outputDir
  createDirectoryIfMissing True buildDir
  withCurrentDirectory buildDir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
    want $ "clang-clog-compare" : "stats-clog" : "stats-clang" : ["ground.csv", "clog.true.positive.csv", "clog.false.positive.csv", "clog.analysis.csv"]
    rules opts

extractReportXML :: ArrowXml a => a XmlTree (Report)
extractReportXML = deep (isElem >>> hasName "file") >>>
  proc x -> do
    file <- getAttrValue "path" -< x
    flaw <- deep (isElem >>> hasName "flaw") -< x
    flawLine <- getAttrValue "line" -< flaw
    flawName <- getAttrValue "name" -< flaw
    returnA -< simpleReport file ((read flawLine)::Int) 0 flawName OtherReport

extractReportsXML :: FilePath -> IO [Report]
extractReportsXML f = runX (readDocument [withValidate no] f >>> extractReportXML)


clean :: JulietOpts -> IO ()
clean opts = do
  let buildDir = opts.dir </> opts.outputDir
  createDirectoryIfMissing True buildDir
  withCurrentDirectory buildDir$ shake shakeOptions {shakeVerbosity=Verbose} $ do
    want ["clean"]
    rules opts
