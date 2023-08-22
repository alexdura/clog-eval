{-# LANGUAGE OverloadedRecordDot #-}

module ClangTidy where

import Report
import Data.List.Split
import Data.Maybe
import Text.Regex.TDFA



processClangTidyOutput :: [String] -> [Report ClangTag]
processClangTidyOutput outs = let chunks = split (dropInitBlank $ keepDelimsL $ whenElt (isJust . extractReportClang)) outs in
                                (\(h:t) -> let r = fromJust $ extractReportClang h in r { desc = foldl (++) r.desc t}) <$> chunks


extractReportClang :: String -> Maybe (Report ClangTag)
extractReportClang l = do
  (_, _, _, [f, l, c, k, m]) <- l =~~ "(.+):(.+):(.+): (.+): (.+)" :: Maybe (String, String, String, [String])
  return $ Report f (read l) (read c) m (case k of
                                           "warning" -> WarningReport
                                           "error" -> ErrorReport
                                           _ -> OtherReport)

extractChecker :: Report ClangTag -> Maybe String
extractChecker (Report _ _ _ desc WarningReport) = do
  (_, _, _, m:_) <- desc =~~ "\\[([^[:space:]]+)\\]" :: Maybe (String, String, String, [String])
  return m
extractChecker _ = Nothing


classifyReport :: Report ClangTag -> ReportClass
classifyReport r = if (r.kind == WarningReport || r.kind == ErrorReport) &&
                      ((r.desc =~ "function call argument is an uninitialized value(.*)\\[clang-analyzer-core.CallAndMessage]") ||
                        (r.desc =~ "Dereference of undefined pointer value(.*)\\[clang-analyzer-unix.Malloc]"))
                   then CWE457
                   else NotRelevant
