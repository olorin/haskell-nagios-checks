{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where

import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Nagios.Check.RabbitMQ
import           Network.HTTP.Conduit       (simpleHttp)
import           System.Nagios.Plugin

checkExchange :: MessageDetail -> CheckOptions -> NagiosPlugin ()
checkExchange MessageDetail{..} CheckOptions{..} = do
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

main :: IO ()
main = do
    opts <- parseOptions
    let hs = case auth opts of
             Just a  -> a ++ "@" ++ hostname opts
             Nothing ->      hostname opts
    let uri = concat ["http://", hs, "/api/exchanges/%2F/", queue opts]

    putStrLn uri

    rawJSON <- simpleHttp uri
--    rawJSON <- liftIO $ BSL.readFile "test/sample_json/tidy_sample_exchange.json"
    let result = processExchange rawJSON
    runNagiosPlugin (checkExchange result opts)

