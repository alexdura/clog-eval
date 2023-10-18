{-# LANGUAGE OverloadedRecordDot #-}

module Clog (classifyClogReport) where

import Report

classifyClogReport :: Report -> ReportClass

classifyClogReport r | r.desc == "UninitializedMemRead" = Clog_UninitializedMemRead
                     | r.desc == "UninitializedVarUse" = Clog_UninitializedVarUse
                     | r.desc == "UseAfterFree" = Clog_UseAfterFree
                     | r.desc == "OSCommandInjection" = Clog_OSCommandInjection
                     | r.desc == "NullPointerDereference" = Clog_NullPointerDereference
                     | r.desc == "UncontrolledFormatString" = Clog_UncontrolledFormatString
