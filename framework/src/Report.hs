module Report (Report (..), ReportKind (..), ReportClass (..), reportIntersect, reportDiff) where

import Data.List
import GHC.Exts (the)
import GHC.ExecutionStack (Location(functionName))

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

data ReportClass = Juliet_CWE457
                 | Juliet_CWE416
                 | Clog_UninitializedVarUse
                 | Clog_UninitializedMemRead
                 | Clog_UseAfterFree
                 | Clang_UninitializedCallArgument
                 | Clang_DereferenceOfUndefinedPointerValue
                 | Clang_PotentialLeakOfMemory
                 | Clang_UseAfterFree
                 | Clang_DeadStores
                 | Clang_DeprecatedOrUnsafeBufferHandling
                 | NotRelevant
                 deriving (Eq, Show, Ord)

reportClassEq :: ReportClass -> ReportClass -> Bool

reportClassEq Juliet_CWE457 Clog_UninitializedMemRead = True
reportClassEq Juliet_CWE457 Clog_UninitializedVarUse = True
reportClassEq Juliet_CWE457 Clang_UninitializedCallArgument = True
reportClassEq Juliet_CWE457 Clang_DereferenceOfUndefinedPointerValue = True
reportClassEq Juliet_CWE416 Clang_UseAfterFree = True
reportClassEq Juliet_CWE416 Clog_UseAfterFree = True

reportClassEq r1 r2
  | r1 > r2 = reportClassEq r2 r1
  | r1 == r2 = True
  | otherwise = False

reportEq ::(Report -> ReportClass) -> (Report -> ReportClass) ->  Report -> Report -> Bool
reportEq cl cr l r = file l == file r && line l == line r && reportClassEq (cl l) (cr r)


reportDiff :: (Report -> ReportClass) -> (Report -> ReportClass) -> [Report] -> [Report] -> [Report]
-- deleteFirstBy internally flips the arguments to the equality function and our function is not symmetric
reportDiff cl cr = deleteFirstsBy (reportEq cr cl)

reportIntersect :: (Report -> ReportClass) -> (Report -> ReportClass) -> [Report] -> [Report] -> [Report]
reportIntersect cl cr = intersectBy (reportEq cl cr)
