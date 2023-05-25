module ClangTidy where


import Data.Time.Clock.System
import System.Process


-- runClangTidy :: FilePath -> Maybe
-- runClangTidy d = withCurrentDirectory d $ do
--   t0 <- getSystemType
--   let command = proc "clang-tidy" "-p"
