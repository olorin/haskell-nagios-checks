--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Check
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Data.Text.Lazy       (Text)
import           Formatting
import           Options.Applicative
import           System.Nagios.Plugin

parsePluginOpts :: Parser PluginOpts
parsePluginOpts =
    PluginOpts <$> parseWarnMagnitude
               <*> parseCritMagnitude
               <*> parseWarnUnusable
               <*> parseCritUnusable
  where
    magnitude = fmap Magnitude auto
    parseWarnMagnitude = option magnitude $
        long "warn-magnitude"
        <> short 'w'
        <> value 5
        <> showDefault
        <> help "Magnitude on which to warn if it reaches warning or critical levels (probably 0-10)"

    parseCritMagnitude = option magnitude $
        long "crit-magnitude"
        <> short 'c'
        <> value 4
        <> showDefault
        <> help "Magnitude on which to warn or go critical on warning or critical levels"

    parseWarnUnusable = option auto $
        long "unusable-warn"
        <> short 'x'
        <> value 0.9
        <> showDefault
        <> help "Unusable index on which to warn [0-1] (closed interval)"

    parseCritUnusable = option auto $
        long "unusable-crit"
        <> short 'y'
        <> value 1
        <> showDefault
        <> help "Unusable index on which to go critical"

validatePluginOpts :: PluginOpts -> Except Text PluginOpts
validatePluginOpts opts =  do
    validate warnMagnitude "warn-magnitude" (<) "<" 0
    validate critMagnitude "crit-magnitude" (<) "<" 0
    validate warnUnusable  "unusable-warn"  (<) "<" 0
    validate warnUnusable  "unusable-warn"  (>) ">" 1
    validate critUnusable  "unusable-crit"  (<) "<" 0
    validate critUnusable  "unusable-crit"  (>) ">" 1
    return opts
  where
    validate lense name p operator v = do
        let checked = opts ^. lense
        when (checked `p` v)
             (throwError $ format ( text
                                  % " failed predicate: "
                                  % prec 3
                                  % " "
                                  % text
                                  %  " "
                                  % prec 3
                                  ) name checked operator v)

main :: IO ()
main = do
    let impl = debugFSImpl "/sys/kernel/debug/extfrag/unusable_index"
                           "/sys/kernel/debug/extfrag/extfrag_index"
    x <- execParser $ info (helper <*> parsePluginOpts) fullDesc
    case runExcept (validatePluginOpts x) of
        Left e ->
            fprint ("Failed to validate option(s): " % text % "\n") e
        Right opts -> runNagiosPlugin (plugin impl opts)
