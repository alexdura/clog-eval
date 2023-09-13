module Clog (classifyClogReport) where

import Report

classifyClogReport :: Report -> ReportClass
classifyClogReport (Report _ _ _ "UninitializedMemRead" _) = Clog_UninitializedMemRead
classifyClogReport (Report _ _ _ "UninitializedVarUse" _) = Clog_UninitializedVarUse
classifyClogReport (Report _ _ _ "UseAfterFree" _) = Clog_UseAfterFree
classifyClogReport (Report _ _ _ "OSCommandInjection" _) = Clog_OSCommandInjection
