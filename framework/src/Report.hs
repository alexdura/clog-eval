module Report (Report (..), ReportKind (..), ReportClass (..), ReportClassifier,
               reportIntersect, reportDiff, extractReportsCSV, simpleReport, reportOverlap) where

import Data.List
import Text.CSV

data Report = Report {
  file :: FilePath,
  line :: Int,
  col :: Int,
  endLine :: Int,
  endCol :: Int,
  desc :: String,
  kind :: ReportKind
  } deriving (Eq, Show)

simpleReport :: FilePath -> Int -> Int -> String -> ReportKind -> Report
simpleReport f l c d k = Report f l c l c d k

data ReportKind = WarningReport
                | ErrorReport
                | OtherReport
                deriving (Eq, Show)

data ReportClass = CWE457
                 | CWE416
                 | CWE78
                 | CWE476
                 | CWE134
                 | CWE122
                 | CWE121
                 | Clog_UninitializedVarUse
                 | Clog_UninitializedMemRead
                 | Clog_UseAfterFree
                 | Clog_OSCommandInjection
                 | Clog_NullPointerDereference
                 | Clog_UncontrolledFormatString
                 | Clog_StackBufferOverflow
                 | Clang_DeadStores
                 | Clang_DeprecatedOrUnsafeBufferHandling
                 | Clang_DereferenceOfUndefinedPointerValue
                 | Clang_IncompatiblePointerTypes
                 | Clang_InsecureAPIStrcpy
                 | Clang_NullDereference
                 | Clang_PotentialLeakOfMemory
                 | Clang_StackAddressEscape
                 | Clang_UndefinedBinaryOperatorResult
                 | Clang_UninitializedAssign
                 | Clang_UninitializedCallArgument
                 | Clang_UseAfterFree
                 | Clang_NonNullParamChecker
                 | Clang_DiagnosticFormatSecurity
                 | Clang_DiagnosticUnusedVariable
                 | Clang_DiagnosticUninitialized
                 | Clang_OutOfBounds
                 | Clang_ArrayBound
                 | NotRelevant
                 deriving (Eq, Show, Ord)

reportClassEq :: ReportClass -> ReportClass -> Bool

reportClassEq CWE457 Clog_UninitializedMemRead = True
reportClassEq CWE457 Clog_UninitializedVarUse = True
reportClassEq CWE457 Clang_UninitializedCallArgument = True
reportClassEq CWE457 Clang_DereferenceOfUndefinedPointerValue = True
reportClassEq CWE457 Clang_DiagnosticUninitialized = True

reportClassEq CWE416 Clang_UseAfterFree = True
reportClassEq CWE416 Clog_UseAfterFree = True

reportClassEq CWE78 Clog_OSCommandInjection = True

reportClassEq CWE476 Clang_NullDereference = True
reportClassEq CWE476 Clog_NullPointerDereference = True

reportClassEq CWE134 Clang_DiagnosticFormatSecurity = True
reportClassEq CWE134 Clog_UncontrolledFormatString = True

reportClassEq CWE122 Clang_OutOfBounds = True
reportClassEq CWE122 Clang_DeprecatedOrUnsafeBufferHandling = True

reportClassEq CWE121 Clang_OutOfBounds = True
reportClassEq CWE121 Clang_DeprecatedOrUnsafeBufferHandling = True
reportClassEq CWE121 Clang_ArrayBound = True
reportClassEq CWE121 Clog_StackBufferOverflow = True

reportClassEq r1 r2
  | r1 > r2 = reportClassEq r2 r1
  | r1 == r2 = True
  | otherwise = False


mapToCWE c = if CWE457 <= c && c <= CWE476 then c
             else error $ "Unknown mapping to CWE for report class " ++ show c

mapToCWE Clog_UninitializedMemRead = CWE457
mapToCWE Clog_UninitializedVarUse = CWE457
mapToCWE Clog_UseAfterFree = CWE416
mapToCWE Clog_OSCommandInjection = CWE78
mapToCWE Clog_NullPointerDereference = CWE476

mapToCWE Clang_UninitializedCallArgument = CWE457
mapToCWE Clang_DereferenceOfUndefinedPointerValue = CWE457
mapToCWE Clang_UseAfterFree = CWE416
mapToCWE Clang_NullDereference = CWE476

type ReportClassifier = Report -> ReportClass

reportEq ::(Report -> ReportClass) -> (Report -> ReportClass) ->  Report -> Report -> Bool
reportEq cl cr l r = file l == file r && line l == line r && reportClassEq (cl l) (cr r)

reportOverlap :: ReportClassifier -> ReportClassifier -> Report -> Report -> Bool
reportOverlap c1 c2 r1 r2 = file r1 == file r2 && reportClassEq (c1 r1) (c2 r2) &&
                            not (line r1 > endLine r2 || line r2 > endLine r1)

reportSimmetricDiff :: (Report -> ReportClass)
                    -> (Report -> ReportClass)
                    -> [Report]
                    -> [Report]
                    -> ([Report], [Report], [Report])
reportSimmetricDiff = undefined

reportDiff :: (Report -> ReportClass) -> (Report -> ReportClass) -> [Report] -> [Report] -> [Report]
-- deleteFirstBy internally flips the arguments to the equality function and our function is not symmetric
reportDiff cl cr = deleteFirstsBy (reportEq cr cl)

reportIntersect :: (Report -> ReportClass) -> (Report -> ReportClass) -> [Report] -> [Report] -> [Report]
reportIntersect cl cr = intersectBy (reportEq cl cr)

extractReportsCSV :: FilePath -> IO [Report]
extractReportsCSV f = do
  Right csv <- parseCSVFromFile f
  return $ fmap (\[f, l, c, err] -> Report f ((read l)::Int) ((read c)::Int) ((read l)::Int) ((read c)::Int) err OtherReport) $ filter (/= [""]) csv




reportDiff' _ l [] = ([], l, [])
reportDiff' _ [] r = ([], [], r)
reportDiff' k (l:ls) (r:rs) = case compare (k l) (k r) of
                                LT -> let (i, lo, ro) = reportDiff' k ls (r:rs) in (i, l:lo, ro)
                                EQ -> let (i, lo, ro) = reportDiff' k ls rs in (l:i, lo, ro)
                                GT -> let (i, lo, ro) = reportDiff' k (l:ls) rs in (i, lo, r:ro)

reportDiff2 :: Ord a => (Report -> a) -> [Report] -> [Report] -> ([Report], [Report], [Report])
reportDiff2 k l r = reportDiff' k (sortOn k l) (sortOn k r)
