{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}


module Juliet (runClang, runClog, clean, JulietOpts(..), defaultJulietOpts, extractReportsXML) where

import Development.Shake
import Development.Shake.FilePath
import qualified Clang.CompilationDatabase as CDB
import Data.Aeson
import Data.Text ( pack, unpack )
import System.Directory
import Text.CSV
import Text.Regex.TDFA
import Data.Maybe
import Text.XML.HXT.Core
import Text.Printf
import Data.List

------------------------------ Report ------------------------------

data Report = Report {
  file :: FilePath,
  line :: Int,
  col :: Int,
  desc :: String,
  kind :: ReportKind
  } deriving (Eq, Show)

data ReportKind = WarningReport
                | ErrorReport
                | OtherReport
                deriving (Eq, Show)

processClangTidyOutput' r [] = [r]
processClangTidyOutput' r (l:ls) = case extractReportClang l of
                                     Just r' -> (r : processClangTidyOutput' r' ls)
                                     Nothing -> processClangTidyOutput' r { desc = r.desc ++ " " ++ l } ls

processClangTidyOutput :: [String] -> [Report]
processClangTidyOutput [] = []
processClangTidyOutput (l:ls) = case extractReportClang l of
                                   Just r -> processClangTidyOutput' r ls
                                   Nothing -> processClangTidyOutput ls

extractReportClang :: String -> Maybe Report
extractReportClang l = do
  (_, _, _, [f, l, c, k, m]) <- l =~~ "(.+):(.+):(.+): (.+): (.+)" :: Maybe (String, String, String, [String])
  return $ Report f (read l) (read c) m (case k of
                                           "warning" -> WarningReport
                                           "error" -> ErrorReport
                                           _ -> OtherReport)

extractChecker :: Report -> Maybe String
extractChecker (Report _ _ _ desc WarningReport) = do
  (_, _, _, m:_) <- desc =~~ "\\[([^[:space:]]+)\\]" :: Maybe (String, String, String, [String])
  return m
extractChecker _ = Nothing

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
  deriving (Show)

defaultJulietOpts d = JulietOpts d [] [] [] "compiler.jar" "." "." "." "_output"

rules :: JulietOpts -> Rules()
rules opts = do
  -- build the compile commands file needed by the clang static analyzer
  "compile_commands.json" %> \out -> do
    c_files <- getDirectoryFiles "." ["//*.c"]
    absCFiles <- liftIO $ mapM canonicalizePath c_files
    let cdb = buildCommandObject opts.dir opts.includes <$> absCFiles
    liftIO $ encodeFile out cdb

  -- run the Clang static analyzer through clang-tidy
  [opts.outputDir </> "clang.analysis.out", opts.outputDir </> "clang.analysis.time"] &%> \[out, time] -> do
    need ["compile_commands.json"]
    Just cdb <- liftIO (decodeFileStrict "compile_commands.json" :: IO (Maybe CDB.CompilationDatabase))
    let files = CDB.file <$> cdb
    (CmdTime t) <- cmd (FileStdout out)  "clang-tidy" (opts.clangXargs ++ (unpack <$> files))
    writeFile' time (show t)

  -- run the Clog analysis
  [opts.outputDir </> "clog.analysis.csv", opts.outputDir </> "clog.analysis.time"] &%> \[out, time] -> do
    need ["compile_commands.json",
          opts.clogProgramPath]
    Just cdb <- liftIO (decodeFileStrict "compile_commands.json" :: IO (Maybe CDB.CompilationDatabase))
    let files = CDB.file <$> cdb
    (CmdTime t) <- cmd (FileStdout $ opts.outputDir </> "clog.analysis.out") (FileStderr $ opts.outputDir </> "clog.analysis.err")
      "java" "-jar" opts.clogJar "-lang" "c4"
      "-S" (Prelude.foldr1 (\x y -> x ++ ":" ++ y) ((++ ",A") <$> unpack <$> files))
      "-D" opts.outputDir
      opts.clogXargs
      opts.clogProgramPath
    writeFile' time (show t)

  -- clasify the Clog reports between true positive and false positives
  [opts.outputDir </> "clog.true.positive.csv",
   opts.outputDir </> "clog.false.positive.csv",
   opts.outputDir </> "clog.false.negative.csv"] &%> \[tpCsv,fpCsv, fnCsv] -> do
    need [opts.outputDir </> "clog.analysis.csv"]
    r <- liftIO $ extractReportsCSV $ opts.outputDir </> "clog.analysis.csv" -- extract the report produced by clog
    g <- liftIO $ extractReportsXML opts.manifest -- extract the ground truth
    let fg = filter (\r -> r.file =~ opts.manifestFilter) g
        r' = map (\rep -> rep { file = takeFileName rep.file }) r
        (tp, fp, fn) = compareResultsFileLineOnly fg r'
        toCsvLine rl = [file rl, show $ line rl , desc rl]
    writeFile' tpCsv (printCSV $ map toCsvLine tp)
    writeFile' fpCsv (printCSV $ map toCsvLine fp)
    writeFile' fnCsv (printCSV $ map toCsvLine fn)

  -- post-process the output of the Clang static analyzer
  opts.outputDir </> "clang.analysis.csv" %> \out -> do
    need [opts.outputDir </> "clang.analysis.out"]
    lines <- readFileLines $ opts.outputDir </> "clang.analysis.out"
    let reports = fmap (\r -> [file r
                              , show $ line r
                              , show $ col r
                              , fromJust $ extractChecker r
                                ]) $
                  Prelude.filter (\r -> kind r == WarningReport && isJust (extractChecker r))
                  (processClangTidyOutput lines)
    writeFile' out (printCSV reports)

  phony "clang-clog-compare" $ do
    need [opts.outputDir </> "clog.analysis.csv", opts.outputDir </> "clang.analysis.csv"]
    clogReport <- liftIO $ extractReportsCSV $ opts.outputDir </> "clog.analysis.csv"
    clangReport <- liftIO $ extractReportsCSV $ opts.outputDir </> "clang.analysis.csv"
    groundReport <- liftIO $ extractReportsXML opts.manifest
    let normFileName r = r { file = takeFileName r.file }
        clangReport' = normFileName <$> clangReport
        clogReport' = normFileName <$> clogReport
        groundReport' = filter (\r -> file r =~ opts.manifestFilter) groundReport
        groundAndClangNotClog = deleteFirstsBy reportEq (intersectBy reportEq groundReport' clangReport') clogReport'
        clangAndClogNotGround = deleteFirstsBy reportEq (intersectBy reportEq clangReport' clogReport') groundReport'
        clogNotClangNotGround = deleteFirstsBy reportEq clogReport' (unionBy reportEq clangReport' groundReport')
        toCsvLine r = [file r, show $ line r, desc r]
    liftIO $ do
      putStrLn "clang/clog compare is the ground truth"
      putStrLn $ printf "True positives reported by clang and not clog %d" (length groundAndClangNotClog)
      putStrLn $ printf "False positives where both clang and clog agree %d" (length clangAndClogNotGround)
      putStrLn $ printf "False positives reported by clog and not clang %d" (length clogNotClangNotGround)
    writeFile' (opts.outputDir </> "ground-and-clang-not-clog.csv") (printCSV $ map toCsvLine groundAndClangNotClog)
    writeFile' (opts.outputDir </> "clang-and-clog-not-ground.csv") (printCSV $ map toCsvLine clangAndClogNotGround)
    writeFile' (opts.outputDir </> "clog-not-clang-not-ground.csv") (printCSV $ map toCsvLine clogNotClangNotGround)


  -- phony "stats-clog"
  printStats opts "clog.analysis.csv" "stats-clog"

  -- phony "stats-clang"
  printStats opts "clang.analysis.csv" "stats-clang"

  phony "clean" $ do
    removeFilesAfter opts.outputDir ["*"]
    removeFilesAfter "." ["compile_commands.json"]

genStats :: JulietOpts -> String -> String -> String -> String -> Rules ()
genStats opts inReport outTruePos outFalsePos outFalseNeg =
  [opts.outputDir </> outTruePos,
   opts.outputDir </> outFalsePos,
   opts.outputDir </> outFalseNeg] &%> \[tpCsv,fpCsv, fnCsv] -> do
    need [opts.outputDir </> inReport]
    r <- liftIO $ extractReportsCSV $ opts.outputDir </> "clog.analysis.csv" -- extract the report produced by clog
    g <- liftIO $ extractReportsXML opts.manifest -- extract the ground truth
    let fg = filter (\r -> r.file =~ opts.manifestFilter) g
        r' = map (\rep -> rep { file = takeFileName rep.file }) r
        (tp, fp, fn) = compareResultsFileLineOnly fg r'
        toCsvLine rl = [file rl, show $ line rl , desc rl]
    writeFile' tpCsv (printCSV $ map toCsvLine tp)
    writeFile' fpCsv (printCSV $ map toCsvLine fp)
    writeFile' fnCsv (printCSV $ map toCsvLine fn)

printStats :: JulietOpts -> String -> String -> Rules ()
printStats opts inReport name =
  phony name $ do
    need [opts.outputDir </> inReport]
    r <- liftIO $ extractReportsCSV $ opts.outputDir </> inReport -- extract the report produced by clog
    g <- liftIO $ extractReportsXML opts.manifest -- extract the ground truth

    let fg = filter (\r -> r.file =~ opts.manifestFilter) g
        r' = map (\rep -> rep { file = takeFileName rep.file }) r
        (tp, fp, fn) = compareResultsFileLineOnly fg r'
    liftIO $ do
      print name
      print ((printf "True positives %d/%d" (length tp) (length fg)) :: String)
      print ((printf "False positives %d/%d" (length fp) (length fg)) :: String)
      print ((printf "False negatives %d/%d" (length fn) (length fg)) :: String)


runClang :: JulietOpts -> IO ()
runClang opts = withCurrentDirectory opts.dir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
  want [opts.outputDir </> "clang.analysis.out"]
  rules opts

runClog opts = withCurrentDirectory opts.dir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
  -- want $ (opts.outputDir </>) <$> ["clog.true.positive.csv", "clog.false.positive.csv", "clog.analysis.csv"]

  want $ "clang-clog-compare" : "stats-clog" : "stats-clang" : ((opts.outputDir </>) <$> ["clog.true.positive.csv", "clog.false.positive.csv", "clog.analysis.csv"])
  rules opts

extractReportXML :: ArrowXml a => a XmlTree Report
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
clean opts = withCurrentDirectory opts.dir $ shake shakeOptions {shakeVerbosity=Verbose} $ do
  want ["clean"]
  rules opts


reportEq l r = file l == file r && line l == line r

compareResultsFileLineOnly :: [Report] -- ground truth report
                           -> [Report] -- tool reports
                           -> ([Report], -- true positives
                               [Report], -- false positives
                               [Report]) -- false negatives

compareResultsFileLineOnly g t = (intersectBy reportEq t g,
                                  deleteFirstsBy reportEq t g,
                                  deleteFirstsBy reportEq g t)
