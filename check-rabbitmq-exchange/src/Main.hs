{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe
import           Data.Monoid
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


    -- Get the length of the response at /api/connections (perfdata only)
    let connUrl = concat [ baseUrl, "/connections" ]
    connRequest <- applyBasicAuth username password <$> parseUrl connUrl

    resp' <- catch (httpLbs connRequest manager)
        (\e -> do let err = show (e :: HttpException)
                  runNagiosPlugin $ addResult Unknown $ T.pack err
                  exitWith (ExitFailure 3)
        )
    let connCount = checkConnCount (responseBody resp')


    -- Full Exchange rates check
    let rateUrl = concat [ baseUrl, "/exchanges/%2F/", exchange opts ]
    authedRequest <- applyBasicAuth username password <$> parseUrl rateUrl

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


    -- Smash together to return all perf data
    checkRawExchange (responseBody resp) opts connCount

