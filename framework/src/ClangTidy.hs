{-# LANGUAGE OverloadedRecordDot #-}

module ClangTidy where

import Report
import Data.List.Split
import Data.Maybe
import Text.Regex.TDFA



processClangTidyOutput :: [String] -> [Report]
processClangTidyOutput outs = let chunks = split (dropInitBlank $ keepDelimsL $ whenElt (isJust . extractReportClang)) outs in
                                (\(h:t) -> let r = fromJust $ extractReportClang h in r { desc = foldl (++) r.desc t}) <$> chunks


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

classifyClangReport :: Report -> ReportClass
classifyClangReport r
  | r.desc =~ "function call argument is an uninitialized value(.*)\\[clang-analyzer-core.CallAndMessage]" = Clang_UninitializedCallArgument
  | r.desc =~ "Dereference of undefined pointer value(.*)\\[clang-analyzer-core.NullDereference]" = Clang_DereferenceOfUndefinedPointerValue
  | r.desc =~ "Potential leak of memory(.*)\\[clang-analyzer-unix.Malloc\\]" = Clang_PotentialLeakOfMemory
  | r.desc =~ "Access to field (.*) results in a dereference of an undefined\
              \ pointer value (.*)\\[clang-analyzer-core.NullDereference\\]" = Clang_DereferenceOfUndefinedPointerValue
  | otherwise = error $ "Can't classify Clang report: '" ++ r.desc ++ "'"
