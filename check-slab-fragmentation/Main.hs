--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

module Main where

import Check
import System.Nagios.Plugin

main :: IO ()
main = do
    let impl = debugFSImpl "/sys/kernel/debug/extfrag/unusable_index"
                           "/sys/kernel/debug/extfrag/extfrag_index"
    let opts = PluginOpts 4 4 0.1 0.2
    runNagiosPlugin (plugin impl opts)
