{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe
import           Data.Monoid
import           Data.Aeson
import qualified Data.Text             as T
import           Nagios.Check.RabbitMQ
import           Network.HTTP.Client
import           System.Environment
import           System.Exit
import           System.Nagios.Plugin  (CheckStatus (..), addResult,
                                        runNagiosPlugin)

main :: IO ()
main = do
    opts <- parseOptions

    username <- maybe "" BSC.pack <$> lookupEnv "RABBIT_USER"
    password <- maybe "" BSC.pack <$> lookupEnv "RABBIT_PASS"

    manager <- newManager defaultManagerSettings

    let baseUrl = concat [ "http://", hostname opts, "/api" ]


    -- Connection count
    let connUrl = concat [ baseUrl, "/connections" ]
    connRequest <- applyBasicAuth username password <$> parseUrl connUrl

    resp' <- catch (httpLbs connRequest manager)
        (\e -> do let err = show (e :: HttpException)
                  runNagiosPlugin $ addResult Unknown $ T.pack err
                  exitWith (ExitFailure 3)
        )
--    print $ length $ parseJSON . resp'

--    let connCount = checkConnectionCount (responseBody resp')
--    let connCount = length . decode (responseBody resp')
    checkConnCount (responseBody resp')

--    addPerfDatum "connectionCount" (RealValue connCount) NullUnit Nothing Nothing Nothing Nothing
--    checkConnLength (responseBody resp) opts

    -- Full Exchange rates check
    let rateUrl = concat [ baseUrl, "/exchanges/%2F/", exchange opts ]
    authedRequest <- applyBasicAuth username password <$> parseUrl baseUrl

    let q_params = [ ("lengths_age",    Just "60")
                   , ("msg_rates_age",  Just "60")
                   , ("msg_rates_incr", Just "60")
                   ]
    let q_authedRequest = setQueryString q_params authedRequest


    resp <- catch (httpLbs q_authedRequest manager)
    	(\e -> do let err = show (e :: HttpException)
		  runNagiosPlugin $ addResult Unknown $ T.pack err
		  exitWith (ExitFailure 3)
        )

    checkRawExchange (responseBody resp) opts

