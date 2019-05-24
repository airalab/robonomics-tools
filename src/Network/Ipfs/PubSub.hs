{-# LANGUAGE OverloadedStrings #-}
module Network.Ipfs.PubSub where

import           Control.Monad           (forever, (<=<))
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Trans     (lift)
import           Data.Aeson              (FromJSON (..), eitherDecode,
                                          withObject, (.:))
import           Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as C8 (hGetLine)
import           Data.ByteString.Lazy    (fromStrict)
import qualified Data.Text               as T (pack)
import           Data.Text.Encoding      (encodeUtf8)
import           Pipes                   (Producer, yield)
import           System.Process          (CreateProcess (std_out),
                                          StdStream (CreatePipe), createProcess,
                                          proc)

newtype MessagePayload
  = MessagePayload { unMessagePayload :: ByteString }
  deriving (Eq, Show)

instance FromJSON MessagePayload where
    parseJSON = withObject "pubsub message" $
        \v -> MessagePayload . encodeUtf8 <$> v .: "data"

decodeRawMsg :: ByteString -> Either String ByteString
decodeRawMsg = convertFromBase Base64 . unMessagePayload
            <=< eitherDecode . fromStrict

-- | Subscribe to topic, decode payload and provide a source of JSON values
subscribe :: MonadIO m
          => String
          -- ^ IPFS api URI
          -> String
          -- ^ IPFS topic
          -> Producer ByteString m ()
          -- ^ JSON payload value stream
subscribe ipfsApi topic = do
    let ipfsProc = proc "ipfs" ["--api", ipfsApi, "pubsub", "sub", "--enc", "json", "--discover", topic]
    hout <- liftIO $ do
        (_, Just h, _, _) <- createProcess ipfsProc { std_out = CreatePipe }
        return h

    forever $ do
        raw_msg <- liftIO $ C8.hGetLine hout
        case decodeRawMsg raw_msg of
            Left e    -> liftIO $ putStrLn e
            Right msg -> yield msg
