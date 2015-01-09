{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Monad.Trans
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy.Char8         as BSL

import           Nagios.Check.RabbitMQ
import           System.Nagios.Plugin


checkExchange :: MessageDetail -> CheckOptions -> NagiosPlugin()
checkExchange result opts = do
    addResult OK "Exchange rate within bounds"

    addPerfDatum "confirm"    (IntegralValue (confirm result)    ) NullUnit Nothing Nothing Nothing Nothing
    addPerfDatum "publishIn"  (IntegralValue (publishIn result)  ) NullUnit Nothing Nothing Nothing Nothing
    addPerfDatum "publishOut" (IntegralValue (publishOut result) ) NullUnit Nothing Nothing Nothing Nothing

    --- Check options, if available
    case (minwarning opts) of
        Just x -> 
            if x > (confirm result) 
	    then addResult Warning "Confirm Rate out of bounds"
	    else return ()
	Nothing -> return ()


main :: IO ()
main = do
    opts <- parseOptions
    rawJSON <- liftIO $ BSL.readFile "test/sample_json/tidy_sample_exchange.json"
    let result = processExchange rawJSON
    runNagiosPlugin (checkExchange result opts)

