{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Nagios.Check.RabbitMQ.Process where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy.Char8  (ByteString)
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Maybe

import           Nagios.Check.RabbitMQ.Types
import           System.Nagios.Plugin

checkRawExchange :: ByteString -> CheckOptions -> IO ()
checkRawExchange bs opts = case eitherDecode bs of
    Left  e -> putStrLn $ "Decoding failed with: " ++ e
    Right x -> checkExchange x opts

checkExchange :: MessageDetail -> CheckOptions -> IO ()
checkExchange MessageDetail{..} CheckOptions{..} = runNagiosPlugin $ do
    addResult OK "Exchange rate within bounds"
    addPerfDatum "rateConfirms"   (RealValue rateConfirms)   NullUnit Nothing Nothing Nothing Nothing
    addPerfDatum "ratePublishIn"  (RealValue ratePublishIn)  NullUnit Nothing Nothing Nothing Nothing
    addPerfDatum "ratePublishOut" (RealValue ratePublishOut) NullUnit Nothing Nothing Nothing Nothing

    --- Check options, if available
    case minWarning of
        Just x  -> when (rateConfirms < x)
                        (addResult Warning "Confirm Rate out of bounds")
        Nothing -> return ()

    case minCritical of
        Just x -> when (rateConfirms < x)
                       (addResult Critical "Confirm Rate out of bounds")
        Nothing -> return ()

    case maxWarning of
        Just x  -> when (rateConfirms > x)
                        (addResult Warning "Confirm Rate out of bounds")
        Nothing -> return ()

    case maxCritical of
        Just x -> when (rateConfirms > x)
                       (addResult Critical "Confirm Rate out of bounds")
        Nothing -> return ()
