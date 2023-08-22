module Clog (classifyReport) where

import Report

classifyReport :: Report ClogTag -> ReportClass
classifyReport (Report _ _ _ "core.uninitialized.Assign" _) = CWE457
classifyReport _ = NotRelevant
