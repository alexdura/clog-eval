module Report (Report (..), ReportKind (..), ReportClass (..), reportIntersect, reportDiff) where

import Data.List

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
                 | Juliet_CWE78
                 | Juliet_CWE476
                 | Clog_UninitializedVarUse
                 | Clog_UninitializedMemRead
                 | Clog_UseAfterFree
                 | Clog_OSCommandInjection
                 | Clog_NullPointerDereference
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
                 | NotRelevant
                 deriving (Eq, Show, Ord)

reportClassEq :: ReportClass -> ReportClass -> Bool

reportClassEq Juliet_CWE457 Clog_UninitializedMemRead = True
reportClassEq Juliet_CWE457 Clog_UninitializedVarUse = True
reportClassEq Juliet_CWE457 Clang_UninitializedCallArgument = True
reportClassEq Juliet_CWE457 Clang_DereferenceOfUndefinedPointerValue = True
reportClassEq Juliet_CWE416 Clang_UseAfterFree = True
reportClassEq Juliet_CWE416 Clog_UseAfterFree = True
reportClassEq Juliet_CWE78 Clog_OSCommandInjection = True
reportClassEq Juliet_CWE476 Clang_NullDereference = True
reportClassEq Juliet_CWE476 Clog_NullPointerDereference = True

reportClassEq r1 r2
  | r1 > r2 = reportClassEq r2 r1
  | r1 == r2 = True
  | otherwise = False


mapToCWE c = if Juliet_CWE457 <= c && c <= Juliet_CWE476 then c
             else error $ "Unknown mapping to CWE for report class " ++ show c

mapToCWE Clog_UninitializedMemRead = Juliet_CWE457
mapToCWE Clog_UninitializedVarUse = Juliet_CWE457
mapToCWE Clog_UseAfterFree = Juliet_CWE416
mapToCWE Clog_OSCommandInjection = Juliet_CWE78
mapToCWE Clog_NullPointerDereference = Juliet_CWE476

mapToCWE Clang_UninitializedCallArgument = Juliet_CWE457
mapToCWE Clang_DereferenceOfUndefinedPointerValue = Juliet_CWE457
mapToCWE Clang_UseAfterFree = Juliet_CWE416
mapToCWE Clang_NullDereference = Juliet_CWE476



reportKey :: (Report -> ReportClass) -> Report -> (FilePath, Int, ReportClass)
reportKey classify r = (file r, line r, mapToCWE $ classify r)

reportEq ::(Report -> ReportClass) -> (Report -> ReportClass) ->  Report -> Report -> Bool
reportEq cl cr l r = file l == file r && line l == line r && reportClassEq (cl l) (cr r)


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
