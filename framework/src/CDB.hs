{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CDB (adjustCompileCommands) where

import Data.Maybe
import Clang.CompilationDatabase
import qualified Data.Text as Text




adjustCompileCommands :: [Text.Text] -> [Text.Text] -> CompilationDatabase -> CompilationDatabase
adjustCompileCommands extraArgs postArgs = map  $ (\c -> c {arguments = c.arguments >>= Just . adjustArgs extraArgs postArgs}) . processCommandObject

-- processCompileCommands :: CompilationDatabase -> CompilationDatabase
-- processCompileCommands = map processCommandObject

processCommandObject :: CommandObject -> CommandObject
processCommandObject c = if isJust $ c.command then c {command = Nothing,
                                                       arguments = Just $ Text.words $ fromJust c.command}
                         else c

adjustArgs :: [Text.Text] -> [Text.Text] -> [Text.Text] -> [Text.Text]
adjustArgs extraArgs postArgs = (++ postArgs) . concatMap (\arg -> case arg of
                                                               "gcc" -> "clang" : extraArgs
                                                               "g++" -> "clang++" : extraArgs
                                                               "clang" -> "clang" : extraArgs
                                                               "clang++" -> "clang++" : extraArgs
                                                               _ -> [arg])
