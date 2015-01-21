{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Nagios.Check.RabbitMQ.Process where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy.Char8  (ByteString)
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Maybe
import qualified Data.Text                   as T
import           Nagios.Check.RabbitMQ.Types
import           System.Nagios.Plugin

checkRawExchange :: ByteString -> CheckOptions -> IO ()
checkRawExchange bs opts = case eitherDecode bs of
    Left  e -> runNagiosPlugin $ addResult Warning $ T.pack ( "Decode failed with: " ++ e )
    Right x -> checkExchange x opts

checkExchange :: MessageDetail -> CheckOptions -> IO ()
checkExchange MessageDetail{..} CheckOptions{..} = runNagiosPlugin $ do
    addResult OK "Exchange rate within bounds"
    addPerfDatum "rateConfirms"   (RealValue rateConfirms)   NullUnit Nothing Nothing Nothing Nothing
    addPerfDatum "ratePublishIn"  (RealValue ratePublishIn)  NullUnit Nothing Nothing Nothing Nothing
    addPerfDatum "ratePublishOut" (RealValue ratePublishOut) NullUnit Nothing Nothing Nothing Nothing

    --- Check options, if available
    unless (rateConfirms `inBoundsOf` minWarning &&
            rateConfirms `inBoundsOf` maxWarning)
           (addResult Warning "Confirm Rate out of bounds")

    unless (rateConfirms `inBoundsOf` minCritical &&
            rateConfirms `inBoundsOf` maxCritical)
           (addResult Critical "Confirm Rate out of bounds")

