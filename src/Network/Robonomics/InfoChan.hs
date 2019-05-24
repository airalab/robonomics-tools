{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Robonomics.InfoChan where

import           Control.Applicative        ((<|>))
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Logger       (MonadLogger, logError)
import           Control.Monad.Trans        (lift)
import           Data.Aeson                 (Value, eitherDecode, parseJSON)
import           Data.Aeson.Types           (parseEither)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Either                (rights)
import qualified Data.Text                  as T (pack)
import           Pipes                      (Producer, yield, (~>))

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
          -> Producer Msg m ()
subscribe api = PubSub.subscribe api ~> parser
  where
    parser msg = case parseMsg =<< eitherDecode (fromStrict msg) of
        Right msg -> yield msg
        Left e    -> lift ($logError $ T.pack e)
