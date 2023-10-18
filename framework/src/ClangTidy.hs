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
  return $ simpleReport f (read l) (read c) m (case k of
                                                  "warning" -> WarningReport
                                                  "error" -> ErrorReport
                                                  _ -> OtherReport)

extractChecker :: Report -> Maybe String
extractChecker (Report _ _ _ _ _ desc WarningReport) = do
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
  | r.desc =~ "Use of memory after it is freed \\[clang-analyzer-unix.Malloc\\]" = Clang_UseAfterFree
  | r.desc =~ "Value stored to (.*) is never read \\[clang-analyzer-deadcode.DeadStores\\]" = Clang_DeadStores
  | r.desc =~ "Call to function (.*) is insecure as it does not provide security checks (.*) \\[clang-analyzer-security.insecureAPI.DeprecatedOrUnsafeBufferHandling\\]" = Clang_DeprecatedOrUnsafeBufferHandling
  | r.desc =~ "Array access (.*) results in a null pointer dereference \\[clang-analyzer-core.NullDereference\\]" = Clang_NullDereference
  | r.desc =~ "Access to field (.*) results in a dereference of a null pointer (.*)\\[clang-analyzer-core.NullDereference\\]" = Clang_NullDereference
  | r.desc =~ "Array access (.*) results in an undefined pointer dereference \\[clang-analyzer-core.NullDereference\\]" = Clang_NullDereference
  | r.desc =~ "CWE-119.*\\[clang-analyzer-security.insecureAPI.strcpy\\]" = Clang_InsecureAPIStrcpy
  | r.desc =~ "Address of stack memory associated with local variable .* is still referred to by the global variable .* upon returning to the caller.  This will be a dangling reference \\[clang-analyzer-core.StackAddressEscape\\]" = Clang_StackAddressEscape
  | r.desc =~ "\\[clang-diagnostic-incompatible-pointer-types\\]" = Clang_IncompatiblePointerTypes
  | r.desc =~ "The (.*) operand of (.*) is a garbage value \\[clang-analyzer-core.UndefinedBinaryOperatorResult\\]" = Clang_UndefinedBinaryOperatorResult
  | r.desc =~ "Assigned value is garbage or undefined \\[clang-analyzer-core.uninitialized.Assign\\]" = Clang_UninitializedAssign
  | r.desc =~ "Dereference of null pointer (.*) \\[clang-analyzer-core.NullDereference\\]" = Clang_NullDereference
  | r.desc =~ "Null pointer passed to (.*) \\[clang-analyzer-core.NonNullParamChecker\\]" = Clang_NonNullParamChecker
  | r.desc =~ "format string is not a string literal (.*)\\[clang-diagnostic-format-security\\]" = Clang_DiagnosticFormatSecurity
  | otherwise = error $ "Can't classify Clang report: '" ++ r.desc ++ "'"
