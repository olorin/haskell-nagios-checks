--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Check
(
    SLABIndex(..),
    node,
    zone,
    magnitudes,
    Unusable,
    Extfrag,
    CheckImpl(..),
    PluginOpts(..),
    Magnitude(..),
    warnMagnitude,
    critMagnitude,
    warnUnusable,
    critUnusable,
    debugFSImpl,
    parseSLABIndex,
    plugin
) where

import Control.Applicative
import Control.Lens hiding (coerce)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.String
import Data.Text (Text)
import Formatting (prec, sformat, shown, stext, (%))
import System.Nagios.Plugin
import Text.Trifecta

data Unusable
data Extfrag

newtype Node = Node Integer
  deriving (Num, Eq)

instance Show Node where
    show (Node n) = "Node " ++ show n

newtype Magnitude = Magnitude Int
  deriving (Ord, Eq, Enum, Num, Real, Integral)

instance Show Magnitude where
    -- assumes 4096 byte pages.
    show (Magnitude n) =
           "magnitude "
        <> show n
        <> " ("
        <> show (2 ^ (12 + n) :: Int)
        <> " KiB)"

data SLABIndex tag = SLABIndex
    { _node       :: Node
    , _zone       :: Text
    , _magnitudes :: [Maybe Double]
    }
  deriving (Eq, Show)
makeLenses ''SLABIndex

data PluginOpts = PluginOpts
    { _warnMagnitude :: Magnitude
    , _critMagnitude :: Magnitude
    , _warnUnusable  :: Double
    , _critUnusable  :: Double
    }
makeLenses ''PluginOpts

checkMagnitudes
    :: Text         -- ^ Index type (unusable, extfrag)
    -> Magnitude            -- ^ Magnitude to check
    -> Double         -- ^ Threshold to trigger on
    -> CheckStatus    -- ^ Failure CheckStatus
    -> [Maybe Double] -- ^ Magnitudes to ceck
    -> NagiosPlugin ()
checkMagnitudes ix_type magnitude threshold status ms =
    case ms ^? ix (fromIntegral magnitude) of
        Nothing ->
            addResult Unknown $ sformat
                ( "Could not retrieve "
                % stext
                % " index of "
                % shown
                % "/"
                % shown
                ) ix_type magnitude (lengthOf traversed ms)
        Just m ->
            case m of
                Nothing ->
                    -- | This should mean that a magnitude value was -1 and
                    -- there's nothing to worry about. We don't expect this
                    -- when checking unusable indices, but I've decided it's
                    -- nothing worth exploding over.
                    return ()
                Just v ->
                    when (v >= threshold) $
                        addResult status $ sformat
                            ( stext
                            % " index of "
                            % shown
                            % " above threshold: "
                            % prec 3
                            % " >= "
                            % prec 3
                            ) ix_type magnitude v threshold

plugin :: CheckImpl -> PluginOpts -> NagiosPlugin ()
plugin impl opts = do
    unusable <- liftIO $ getUnusableIndices impl
    extfrag <- liftIO $ getExtfragIndices impl

    let forUnusable = forOf_ (traversed . magnitudes) unusable
    let forExtfrag = forOf_ (traversed . magnitudes) extfrag

    -- Only warn for warning magnitude unusables, even if this magnitude goes
    -- past the critical threshold.
    forUnusable $ checkMagnitudes "unusable"
                                  (opts ^. warnMagnitude)
                                  (opts ^. warnUnusable)
                                  Warning

    -- Also warn for critical magnitude unusables
    forUnusable $ checkMagnitudes "unusable"
                                  (opts ^. critMagnitude)
                                  (opts ^. warnUnusable)
                                  Warning

    -- And go gritical if those pass the critical threshold
    forUnusable $ checkMagnitudes "unusable"
                                  (opts ^. critMagnitude)
                                  (opts ^. critUnusable)
                                  Critical

    -- Any extfrag reading is bad at the given magnitude. It means that an
    -- allocation would have failed.
    forExtfrag $ checkMagnitudes "extfrag"
                                 (opts ^. warnMagnitude)
                                 0
                                 Warning

    forExtfrag $ checkMagnitudes "extfrag"
                                 (opts ^. critMagnitude)
                                 0
                                 Critical

    -- Traverse the list of magnitude lists, indexing from 0 for each sub-list
    forOf_ traversed unusable $ \u ->
        forOf_ (magnitudes . itraversed . _Just . withIndex) u $ \(i, m) ->
            let msg = sformat (shown % " " % stext % " " % shown)
                              (u ^. node) (u ^. zone) (Magnitude i)
            in addPerfDatum msg
                            (RealValue (m * 100))
                            Percent
                            Nothing
                            Nothing
                            Nothing
                            Nothing

    let path = traversed . magnitudes . traversed . _Just
    let mean = sumOf path unusable / fromIntegral (lengthOf path unusable)
    addResult OK (sformat ("Mean fragmentation: " % prec 2 % "%") (mean * 100))

-- | An implementation of a "getter" of 'ExtfragIndex's and 'UnusableIndex's.
--
-- Yay, first class modules. This just seems a little more readable and
-- explicit than a typeclass.
data CheckImpl = CheckImpl
    { getUnusableIndices :: IO [SLABIndex Unusable]
    , getExtfragIndices  :: IO [SLABIndex Extfrag]
    }

-- | Grab unusable and extfrag indices from debugfs, specifically probably:
--
-- /sys/kernel/debug/extfrag/unusable_index
-- /sys/kernel/debug/extfrag/extfrag_index
debugFSImpl
    :: FilePath
    -> FilePath
    -> CheckImpl
debugFSImpl unusable_index_path extfrag_index_path =
    CheckImpl (tryParse unusable_index_path)
              (tryParse extfrag_index_path)
  where
    tryParse :: FilePath -> IO [SLABIndex a]
    tryParse path = do
        -- File must be read as lazy bytestring due to stat returning length of
        -- 0 on debugfs files.
        r <- parseByteString (some parseSLABIndex) mempty . L.toStrict <$> L.readFile path
        case r of
            Failure e -> error $ "Failed parsing: " ++ show e
            Success v -> return v

-- | Parse one SLABIndex line, works for both extfrag and unusable_index lines
parseSLABIndex :: Parser (SLABIndex a)
parseSLABIndex =
    -- Node 0, zone   Normal 0.000 0.972 1.000 1.000 1.000 -1.000 ...
    SLABIndex <$> (spaces *> parseNode)
              <*> (char ',' *> spaces *> parseZone)
              <*> many (spaces *> choice [Nothing <$ char '-' <* double, Just <$> double] )
  where
    parseNode = text "Node" *> whiteSpace *> (Node <$> natural)
    parseZone = text "zone" *> whiteSpace *> (fromString <$> many (letter <|> digit))
