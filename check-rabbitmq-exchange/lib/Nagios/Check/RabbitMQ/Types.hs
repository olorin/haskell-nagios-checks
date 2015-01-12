{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Nagios.Check.RabbitMQ.Types where

import qualified Data.ByteString.Char8 as BSC
import           Data.Int
import           Control.Applicative
import           Data.Aeson
import           Data.Text(Text)
import qualified Data.Text as T
import           GHC.Generics

data CheckOptions = CheckOptions
    { hostname    :: String
    , queue       :: String
    , auth        :: Maybe String
    , minWarning  :: Maybe Int64
    , minCritical :: Maybe Int64
    , maxWarning  :: Maybe Int64
    , maxCritical :: Maybe Int64
    } deriving Show

data MessageDetail = MessageDetail
    { rateConfirms   :: Int64
    , ratePublishIn  :: Int64
    , ratePublishOut :: Int64
    } deriving (Show,Generic)

instance FromJSON MessageDetail where
    parseJSON (Object o) = MessageDetail
                        <$> ((o .: "message_stats") >>= (.: "confirm_details") >>= (.: "avg_rate"))
                        <*> ((o .: "message_stats") >>= (.: "publish_in_details") >>= (.: "avg_rate"))
                        <*> ((o .: "message_stats") >>= (.: "publish_out_details") >>= (.: "avg_rate"))

