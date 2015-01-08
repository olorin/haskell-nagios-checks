{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Monad.Trans
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy.Char8         as BSL

import           Nagios.Check.RabbitMQ



main :: IO ()
main = do
    opts <- parseOptions
    rawJSON <- liftIO $ BSL.readFile "test/sample_json/tidy_sample_exchange.json"
    print $ processExchange rawJSON
--    print (processExchange rawJSON)

