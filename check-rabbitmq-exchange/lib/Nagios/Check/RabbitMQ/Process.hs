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
import           Data.Int
import qualified Data.Vector as V

checkRawExchange :: ByteString -> CheckOptions -> IO ()
checkRawExchange bs opts = case eitherDecode bs of
    Left  e -> runNagiosPlugin $ addResult Unknown $ T.pack ( "Decode failed with: " ++ e )
    Right x -> checkExchange x opts

checkConnCount :: ByteString -> IO ()
checkConnCount resp = case eitherDecode resp of
        Right (Array x) -> runNagiosPlugin $ addPerfDatum "connectionCount" (IntegralValue (fromIntegral (V.length x))) NullUnit Nothing Nothing Nothing Nothing
	--perfConnCount x
        Left e -> runNagiosPlugin $ addResult Unknown $ T.pack ( "Decode fialed with: " ++ e )

--perfConnCount :: ConnectionCount -> IO()
--perfConnCount ConnectionCount{..} = runNagiosPlugin $ addPerfDatum "connectionCount" (IntegralValue (fromIntegral (length x))) NullUnit Nothing Nothing Nothing Nothing

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

