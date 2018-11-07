{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Network.Robonomics.Message
  (
    RobonomicsMsg(..)
  , Demand(..)
  , Offer(..)
  , Report(..)
  ) where

import           Crypto.Ethereum          (ecsign)
import           Crypto.Hash              (Digest, Keccak_256)
import qualified Crypto.Hash              as Crypto
import           Crypto.Secp256k1         (CompactRecSig (..), SecKey)
import           Data.Aeson               (FromJSON (..), ToJSON (..),
                                           withObject, (.:))
import qualified Data.ByteArray           as BA (convert, drop)
import           Data.ByteArray.Encoding  (Base (Base16), convertFromBase)
import           Data.ByteArray.HexString (HexString)
import           Data.ByteArray.Sized     (unsafeSizedByteArray)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Base58   (bitcoinAlphabet, decodeBase58,
                                           encodeBase58)
import qualified Data.ByteString.Char8    as C8 (concat, cons, pack, unpack)
import qualified Data.ByteString.Short    as Short (fromShort, pack)
import           Data.Monoid              ((<>))
import           Data.Solidity.Abi.Codec  (encode)
import           Data.Solidity.Prim       (Address, Bytes, BytesN, UIntN)
import           Data.String.Extra        (toLowerFirst)
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)
import           Generics.SOP             (Generic)
import qualified GHC.Generics             as GHC (Generic)
import           GHC.TypeLits

instance (KnownNat n, n <= 256) => FromJSON (UIntN n) where
    parseJSON = fmap fromInteger . parseJSON

instance (KnownNat n, n <= 256) => ToJSON (UIntN n) where
    toJSON = toJSON . toInteger

class RobonomicsMsg a where
    hash :: a -> Digest Keccak_256

    sign :: SecKey -> a -> Bytes
    {-# INLINE sign #-}
    sign key = sigToBytes . ecsign key . hash

data Demand = Demand
    { demandModel        :: !Bytes
    , demandObjective    :: !Bytes
    , demandToken        :: !Address
    , demandCost         :: !(UIntN 256)
    , demandLighthouse   :: !Address
    , demandValidator    :: !Address
    , demandValidatorFee :: !(UIntN 256)
    , demandDeadline     :: !(UIntN 256)
    , demandNonce        :: !(BytesN 32)
    , demandSignature    :: !Bytes
    }
  deriving (Eq, GHC.Generic)

instance Show Demand where
    show Demand{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' <$>
            [ encodeBase58 bitcoinAlphabet (BA.convert demandModel)
            , encodeBase58 bitcoinAlphabet (BA.convert demandObjective)
            , C8.pack (show demandToken)
            , C8.pack (show demandCost)
            , C8.pack (show demandLighthouse)
            , C8.pack (show demandValidator)
            , C8.pack (show demandValidatorFee)
            , C8.pack (show (BA.convert demandNonce :: HexString))
            , C8.pack (show (BA.convert demandSignature :: HexString))
            ]

instance Generic Demand
instance RobonomicsMsg Demand where
    hash = demandHash

instance FromJSON Demand where
    parseJSON = withObject "Demand" $ \v -> Demand
        <$> (b58decode =<< v .: "model")
        <*> (b58decode =<< v .: "objective")
        <*> v .: "token"
        <*> v .: "cost"
        <*> v .: "lighthouse"
        <*> v .: "validator"
        <*> v .: "validatorFee"
        <*> v .: "deadline"
        <*> ((unsafeSizedByteArray :: Bytes -> BytesN 32) <$> (b16decode =<< v .: "nonce"))
        <*> (b16decode =<< v .: "signature")

data Offer = Offer
    { offerModel         :: !Bytes
    , offerObjective     :: !Bytes
    , offerToken         :: !Address
    , offerCost          :: !(UIntN 256)
    , offerValidator     :: !Address
    , offerLighthouse    :: !Address
    , offerLighthouseFee :: !(UIntN 256)
    , offerDeadline      :: !(UIntN 256)
    , offerNonce         :: !(BytesN 32)
    , offerSignature     :: !Bytes
    }
  deriving (Eq, GHC.Generic)

instance Show Offer where
    show Offer{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' <$>
            [ encodeBase58 bitcoinAlphabet (BA.convert offerModel)
            , encodeBase58 bitcoinAlphabet (BA.convert offerObjective)
            , C8.pack (show offerToken)
            , C8.pack (show offerCost)
            , C8.pack (show offerValidator)
            , C8.pack (show offerLighthouse)
            , C8.pack (show offerLighthouseFee)
            , C8.pack (show (BA.convert offerNonce :: HexString))
            , C8.pack (show (BA.convert offerSignature :: HexString))
            ]

instance Generic Offer
instance RobonomicsMsg Offer where
    hash = offerHash

instance FromJSON Offer where
    parseJSON = withObject "Offer" $ \v -> Offer
        <$> (b58decode =<< v .: "model")
        <*> (b58decode =<< v .: "objective")
        <*> v .: "token"
        <*> v .: "cost"
        <*> v .: "validator"
        <*> v .: "lighthouse"
        <*> v .: "lighthouseFee"
        <*> v .: "deadline"
        <*> ((unsafeSizedByteArray :: Bytes -> BytesN 32) <$> (b16decode =<< v .: "nonce"))
        <*> (b16decode =<< v .: "signature")

data Report = Report
  { reportLiability :: !Address
  , reportResult    :: !Bytes
  , reportSuccess   :: !Bool
  , reportSignature :: !Bytes
  } deriving (Eq, GHC.Generic)

instance Show Report where
    show Report{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' <$>
            [ C8.pack (show reportLiability)
            , encodeBase58 bitcoinAlphabet (BA.convert reportResult)
            , C8.pack (show reportSuccess)
            , C8.pack (show (BA.convert reportSignature :: HexString))
            ]

instance Generic Report
instance RobonomicsMsg Report where
    hash = reportHash

instance FromJSON Report where
    parseJSON = withObject "Report" $ \v -> Report
        <$> v .: "liability"
        <*> (b58decode =<< v .: "result")
        <*> v .: "success"
        <*> (b16decode =<< v .: "signature")

demandHash :: Demand -> Digest Keccak_256
{-# INLINE demandHash #-}
demandHash Demand{..} =
    Crypto.hash $ demandModel
               <> demandObjective
               <> BA.drop 12 (encode demandToken)
               <> encode demandCost
               <> BA.drop 12 (encode demandLighthouse)
               <> BA.drop 12 (encode demandValidator)
               <> encode demandValidatorFee
               <> encode demandDeadline
               <> encode demandNonce

offerHash :: Offer -> Digest Keccak_256
{-# INLINE offerHash #-}
offerHash Offer{..} =
    Crypto.hash $ offerModel
               <> offerObjective
               <> BA.drop 12 (encode offerToken)
               <> encode offerCost
               <> BA.drop 12 (encode offerValidator)
               <> BA.drop 12 (encode offerLighthouse)
               <> encode offerLighthouseFee
               <> encode offerDeadline
               <> encode offerNonce

reportHash :: Report -> Digest Keccak_256
{-# INLINE reportHash #-}
reportHash Report{..} =
    Crypto.hash $ BA.drop 12 (encode reportLiability)
               <> reportResult
               <> BA.drop 31 (encode reportSuccess)

b58decode :: Monad m => Text -> m Bytes
b58decode = toError . maybe (Left "Unable to decode base 58") Right
          . fmap BA.convert . decodeBase58 bitcoinAlphabet . encodeUtf8

b16decode :: Monad m => Text -> m Bytes
b16decode = toError . convertFromBase Base16 . encodeUtf8

toError :: Monad m => Either String a -> m a
{-# INLINE toError #-}
toError = either fail return

sigToBytes :: CompactRecSig -> Bytes
sigToBytes (CompactRecSig r s v) = BA.convert $ foldMap Short.fromShort [r, s, Short.pack [v]]
