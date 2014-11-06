--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

import Check
import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import System.Nagios.Plugin
import Test.Hspec
import Text.Trifecta

exUnusableStr :: String
exUnusableStr =
       "Node 0, zone    DMA32 0.000 0.003 0.067 0.128 "
    <> "0.179 0.231 0.290 0.356 0.417 0.525 0.538"

exUnusable :: PageIndex Unusable
exUnusable = PageIndex 0 "DMA32"
    [Just x | x <- [ 0, 0.003, 0.067, 0.128, 0.179, 0.231, 0.290
                   , 0.356, 0.417, 0.525, 0.538 ]]

exExtfragStr :: String
exExtfragStr =
       "Node 0, zone   Normal -1.000 -1.000 -1.000 -1.000 "
    <> "-1.000 -1.000 -1.000 -1.000 -1.000 -1.000 0.998"

exExtfrag :: PageIndex Unusable
exExtfrag = PageIndex 0 "Normal" (replicate 10 Nothing ++ [Just 0.998])

testImpl :: CheckImpl
testImpl = debugFSImpl "./fixtures/unusable_index" "./fixtures/extfrag_index"

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        it "parses unusable page index" $
            shouldParse parsePageIndex exUnusableStr exUnusable

        it "parses multiple lines of page index" $
            let ls = unlines $ replicate 10 exUnusableStr
            in shouldParse (some parsePageIndex) ls (replicate 10 exUnusable)

        it "parses extfrag page index" $
            shouldParse parsePageIndex exExtfragStr exExtfrag

    describe "debugFS impl" $
        it "parses example debugfs files" $ do
            let sumMagnitudes = sumOf (traversed . magnitudes . traversed . _Just)
            sumMagnitudes <$> getUnusableIndices testImpl >>= (`shouldBe` 27.362)
            sumMagnitudes <$> getExtfragIndices testImpl >>= (`shouldBe` 16.094)

    describe "Nagios check" $ do
        it "warns on warning magnitude over warning threshold" $
            -- Magnitude 1 should warn as it is above the threshold
            shouldPlugin (PluginOpts 1 0 0.5 0.9)
                         Warning
                         (  "unusable index of magnitude 1"
                         <> " (8192 KiB) above threshold:"
                         <> " 0.996 >= 0.500"
                         )

        it "warns on critical magnitude over warning threshold" $
            -- Magnitude 1 should warn as it is above the threshold, but not
            -- above the critical one, even though it is a "critical magnitude"
            shouldPlugin (PluginOpts 0 0 0 1)
                         Warning
                         (  "unusable index of magnitude 0"
                         <> " (4096 KiB) above threshold:"
                         <> " 0.00 >= 0.00"
                         )

        it "criticals on critical magnitude over critical threshold" $
            -- This is probably the important one. The bad thing is over the
            -- maximum badness. Test the boundary here too.
            shouldPlugin (PluginOpts 5 4 0.9 1)
                         Critical
                         (  "unusable index of magnitude 4"
                         <> " (65536 KiB) above threshold:"
                         <> " 1.00 >= 1.00"
                         )

        it "warns on extfrag warning magnitude" $
            shouldPlugin (PluginOpts 1 0 1 1)
                         Warning
                         (  "extfrag index of magnitude 1"
                         <> " (8192 KiB) above threshold:"
                         <> " 0.650 >= 0.00"
                         )

        it "warns on extfrag warning magnitude" $
            shouldPlugin (PluginOpts 0 1 1 1)
                         Critical
                         (  "extfrag index of magnitude 1"
                         <> " (8192 KiB) above threshold:"
                         <> " 0.650 >= 0.00"
                         )

        it "has perfdata" $ do
            (_, (_, ps)) <- runNagiosPlugin' $ plugin testImpl (PluginOpts 1 1 1 1)
            length ps `shouldBe` 44 -- 11 page magnitudes * 4

shouldPlugin :: PluginOpts -> CheckStatus -> Text -> Expectation
shouldPlugin opts status info = do
    (_, (rs, _)) <- runNagiosPlugin' $ plugin testImpl opts
    let r = maximum rs
    checkStatus r `shouldBe` status
    checkInfo r `shouldBe` info

shouldParse :: (Show a, Eq a)
            => Parser a
            -> String
            -> a
            -> Expectation
shouldParse parser input expectation =
    case parseString parser mempty input of
        Success r -> r `shouldBe` expectation
        Failure f -> fail $ show f
