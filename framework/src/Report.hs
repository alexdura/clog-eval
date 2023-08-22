module Report (Report (..), ReportKind (..), ReportClass (..), ClangTag, JulietTag, ClogTag) where

data Report a = Report {
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

data ReportClass = CWE457
                 | NotRelevant

data ClangTag
data JulietTag
data ClogTag
