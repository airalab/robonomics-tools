{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Robonomics.InfoChan where

import           Control.Applicative        ((<|>))
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Logger       (MonadLogger, logError)
import           Control.Monad.Trans        (lift)
import           Data.Aeson                 (Value, parseJSON)
import           Data.Aeson.Types           (parseEither)
import           Data.ByteArray.Encoding    (Base (Base64), convertFromBase,
                                             convertToBase)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Either                (rights)
import           Data.Machine               (ProcessT, SourceT, await, mapping,
                                             repeatedly, yield, (~>))
import qualified Data.Text                  as T

import qualified Network.Ipfs.PubSub        as PubSub
import           Network.Robonomics.Message (Demand, Offer, Report)

data Msg = MkDemand !Demand
         | MkOffer  !Offer
         | MkReport !Report
  deriving (Show, Eq)

parseMsg :: Value -> Either String Msg
parseMsg = parseEither $
    \msg -> MkDemand <$> parseJSON msg
        <|> MkOffer  <$> parseJSON msg
        <|> MkReport <$> parseJSON msg

subscribe :: (MonadIO m, MonadLogger m)
          => String
          -- ^ IPFS Api multiaddr
          -> String
          -- ^ IPFS topic name
          -> SourceT m Msg
subscribe ipfsApi topic = PubSub.subscribe ipfsApi topic ~> parser
  where
    parser = repeatedly $ do
        msg <- await
        case parseMsg msg of
            Right msg -> yield msg
            Left e    -> lift ($logError $ T.pack e)
