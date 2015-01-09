module Nagios.Check.RabbitMQ.Process where

import           Data.Aeson
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy.Char8         as BSL

import           Nagios.Check.RabbitMQ.Types


processExchange :: BSL.ByteString -> MessageDetail
processExchange exchange =
    case decode exchange of
        Just x -> x
        Nothing -> MessageDetail [] 
{-
    case eitherDecode exchange of
        Left e  -> return []
        Right x -> x
-}
