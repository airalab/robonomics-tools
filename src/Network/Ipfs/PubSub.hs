{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Ipfs.PubSub where

import           Control.Monad           (forever, (<=<))
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Logger    (MonadLogger, logError)
import           Control.Monad.Trans     (lift)
import           Data.Aeson              (FromJSON (..), Value, eitherDecode,
                                          withObject, (.:))
import           Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as C8 (hGetLine)
import           Data.ByteString.Lazy    (fromStrict)
import           Data.Machine            (SourceT, construct, yield)
import qualified Data.Text               as T (pack)
import           Data.Text.Encoding      (encodeUtf8)
import           System.Process          (CreateProcess (std_out),
                                          StdStream (CreatePipe), createProcess,
                                          proc)

newtype MessagePayload
  = MessagePayload { unMessagePayload :: ByteString }
  deriving Eq

instance FromJSON MessagePayload where
    parseJSON = withObject "pubsub message" $
        \v -> MessagePayload . encodeUtf8 <$> v .: "data"

decodeBase64 :: ByteString -> Either String Value
decodeBase64 = eitherDecode . fromStrict
            <=< convertFromBase Base64 . unMessagePayload
            <=< eitherDecode . fromStrict

-- | Subscribe to topic, decode payload and provide a source of JSON values
subscribe :: (MonadIO m, MonadLogger m)
          => String
          -- ^ IPFS api URI
          -> String
          -- ^ IPFS topic
          -> SourceT m Value
          -- ^ JSON payload value stream
subscribe ipfsApi topic = construct $ do
    let ipfsProc = proc "ipfs" ["--api", ipfsApi, "pubsub", "sub", "--enc", "json", "--discover", topic]
    (_, Just hout, _, _) <- liftIO $ createProcess ipfsProc { std_out = CreatePipe }
    forever $ do
        mbmsg <- decodeBase64 <$> liftIO (C8.hGetLine hout)
        case mbmsg of
            Left e    -> lift ($logError $ T.pack e)
            Right msg -> yield msg
