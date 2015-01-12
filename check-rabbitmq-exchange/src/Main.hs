{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Monoid
import qualified Data.ByteString.Char8 as BSC

import           Nagios.Check.RabbitMQ
import           Network.HTTP.Client
import           System.Environment
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

    username <- maybe "" BSC.pack <$> lookupEnv "RABBIT_USER"
    password <- maybe "" BSC.pack <$> lookupEnv "RABBIT_PASS"

    let baseUrl = concat [ "http://", hostname opts, "/api/exchanges/%2F/", exchange opts ]
    authedRequest <- applyBasicAuth username password <$> parseUrl baseUrl

    let q_params = [ ("lengths_age",    Just "60")
                   , ("msg_rates_age",  Just "60")
                   , ("msg_rates_incr", Just "60")
                   ]
    let q_authedRequest = setQueryString q_params authedRequest

    print $ getUri q_authedRequest

    manager <- newManager defaultManagerSettings
    resp <- httpLbs q_authedRequest manager

    let result = processExchange $ responseBody resp
    runNagiosPlugin (checkExchange result opts)

