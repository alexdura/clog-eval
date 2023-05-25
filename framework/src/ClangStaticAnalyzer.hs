module ClangStaticAnalyzer where

clangAnalyzerPreOpts :: [String]
clangAnalyzerPreOpts = ["--analyze"]
clangAnalyzerPostOpts :: [String]
clangAnalyzerPostOpts = ["-Xanalyzer", "-analyzer-output=sarif"] ++ clangAnalysisOpts
clangAnalysisOpts :: [String]
clangAnalysisOpts =  ["-Xclang", "-analyzer-checker=core.uninitialized"] -- Check for uninitialized values used as array subscripts
                      -- "-analyzer-checker", "core.uninitialized.Assign",     -- Check for assigning uninitialized values
                      -- "-analyzer-checker", "core.uninitialized.Branch", --     Check for uninitialized values used as branch conditions
                      -- "-analyzer-checker", "core.uninitialized.CapturedBlockVariable", --  Check for blocks that capture uninitialized values
                      -- "-analyzer-checker", "core.uninitialized.UndefReturn"]
