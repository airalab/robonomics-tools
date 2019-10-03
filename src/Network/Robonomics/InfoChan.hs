{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Robonomics.InfoChan where

import           Control.Applicative        ((<|>))
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Logger       (MonadLogger, logWarn)
import           Control.Monad.Trans        (lift)
import           Data.Aeson                 (ToJSON (..), Value, eitherDecode,
                                             encode, parseJSON)
import           Data.Aeson.Types           (parseEither)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Either                (rights)
import qualified Data.Text                  as T (pack)
import           Pipes                      (Consumer, Producer, yield, (>->),
                                             (~>))
import qualified Pipes.Prelude              as P (map)

import qualified Network.Ipfs.PubSub        as PubSub
import           Network.Robonomics.Message (Accepted, Demand, Offer, Pending,
                                             Report)

data Msg = MkDemand   !Demand
         | MkOffer    !Offer
         | MkReport   !Report
         | MkAccepted !Accepted
         | MkPending  !Pending
  deriving (Show, Eq)

instance ToJSON Msg where
    toJSON (MkDemand v)   = toJSON v
    toJSON (MkOffer  v)   = toJSON v
    toJSON (MkReport v)   = toJSON v
    toJSON (MkAccepted v) = toJSON v
    toJSON (MkPending v)  = toJSON v

parseMsg :: Value -> Either String Msg
parseMsg = parseEither $
    \msg -> MkDemand   <$> parseJSON msg
        <|> MkOffer    <$> parseJSON msg
        <|> MkReport   <$> parseJSON msg
        <|> MkAccepted <$> parseJSON msg
        <|> MkPending  <$> parseJSON msg

subscribe :: (MonadIO m, MonadLogger m)
          => String
          -- ^ IPFS Api multiaddr
          -> String
          -- ^ IPFS topic name
          -> Producer Msg m ()
subscribe api = PubSub.subscribe api ~> parser
  where
    parser msg = case parseMsg =<< eitherDecode (fromStrict msg) of
        Right msg -> yield msg
        Left e    -> lift ($logWarn ("Unable to parse msg: " <> T.pack (show msg) <> " : " <> T.pack e))

publish :: (MonadIO m, MonadLogger m, ToJSON msg)
        => String
        -- ^ IPFS Api multiaddr
        -> String
        -- ^ IPFS topic name
        -> Consumer msg m ()
publish api topic = P.map (toStrict . encode) >-> PubSub.publish api topic
