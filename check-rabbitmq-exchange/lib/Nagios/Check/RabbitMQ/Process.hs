module Nagios.Check.RabbitMQ.Process where

import           Data.Aeson
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Maybe
import           Nagios.Check.RabbitMQ.Types


processExchange :: BSL.ByteString -> MessageDetail
processExchange exchange = fromMaybe (MessageDetail 0 0 0) (decode exchange)
