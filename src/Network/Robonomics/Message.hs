{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Network.Robonomics.Message
  (
    RobonomicsMsg(..)
  , Demand(..)
  , Offer(..)
  , Report(..)
  ) where

import           Crypto.Ethereum           (PrivateKey, signMessage)
import           Crypto.Hash               (Digest, Keccak_256)
import qualified Crypto.Hash               as Crypto
import           Data.Aeson                (FromJSON (..), ToJSON (..),
                                            withObject, (.:))
import           Data.Base58String.Bitcoin (Base58String, toBytes)
import qualified Data.ByteArray            as BA (convert, drop)
import           Data.ByteArray.Encoding   (Base (Base16), convertFromBase)
import           Data.ByteArray.HexString  (HexString)
import           Data.ByteArray.Sized      (unsafeSizedByteArray)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C8 (concat, cons, pack, unpack)
import qualified Data.ByteString.Short     as Short (fromShort, pack)
import           Data.Maybe                (fromJust)
import           Data.Monoid               ((<>))
import           Data.Proxy                (Proxy (..))
import           Data.Solidity.Abi         (AbiPut (..), AbiType (..))
import           Data.Solidity.Abi.Codec   (encode)
import           Data.Solidity.Prim        (Address, Bytes, BytesN, UIntN)
import           Data.String.Extra         (toLowerFirst)
import           Data.Text                 (Text)
import           Data.Text.Encoding        (encodeUtf8)
import           Generics.SOP              (Generic)
import qualified GHC.Generics              as GHC (Generic)
import           GHC.TypeLits

instance (KnownNat n, n <= 256) => FromJSON (UIntN n) where
    parseJSON = fmap fromInteger . parseJSON

instance (KnownNat n, n <= 256) => ToJSON (UIntN n) where
    toJSON = toJSON . toInteger

instance AbiType a => AbiType (Maybe a) where
    isDynamic _ = isDynamic (Proxy :: Proxy a)

instance AbiPut a => AbiPut (Maybe a) where
    abiPut Nothing  = pure ()
    abiPut (Just a) = abiPut a

instance AbiType Base58String where
    isDynamic _ = True

instance AbiPut Base58String where
    abiPut = abiPut . toBytes

class RobonomicsMsg a where
    hash :: a -> Digest Keccak_256

    sign :: PrivateKey -> a -> Bytes
    {-# INLINE sign #-}
    sign key = signMessage key . hash

data Demand = Demand
    { demandModel        :: !Base58String
    , demandObjective    :: !Base58String
    , demandToken        :: !Address
    , demandCost         :: !(UIntN 256)
    , demandLighthouse   :: !Address
    , demandValidator    :: !Address
    , demandValidatorFee :: !(UIntN 256)
    , demandDeadline     :: !(UIntN 256)
    , demandNonce        :: !(Maybe (UIntN 256))
    , demandSender       :: !Address
    , demandSignature    :: !Bytes
    }
  deriving (Eq, GHC.Generic)

instance Show Demand where
    show Demand{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' <$>
            [ C8.pack (show demandModel)
            , C8.pack (show demandObjective)
            , C8.pack (show demandToken)
            , C8.pack (show demandCost)
            , C8.pack (show demandLighthouse)
            , C8.pack (show demandValidator)
            , C8.pack (show demandValidatorFee)
            , C8.pack (show demandDeadline)
            , C8.pack (show demandNonce)
            , C8.pack (show demandSender)
            , C8.pack (show (BA.convert demandSignature :: HexString))
            ]

instance Generic Demand
instance RobonomicsMsg Demand where
    hash = demandHash

instance FromJSON Demand where
    parseJSON = withObject "Demand" $ \v -> Demand
        <$> v .: "model"
        <*> v .: "objective"
        <*> v .: "token"
        <*> v .: "cost"
        <*> v .: "lighthouse"
        <*> v .: "validator"
        <*> v .: "validatorFee"
        <*> v .: "deadline"
        <*> pure Nothing
        <*> v .: "sender"
        <*> (b16decode =<< v .: "signature")

data Offer = Offer
    { offerModel         :: !Base58String
    , offerObjective     :: !Base58String
    , offerToken         :: !Address
    , offerCost          :: !(UIntN 256)
    , offerValidator     :: !Address
    , offerLighthouse    :: !Address
    , offerLighthouseFee :: !(UIntN 256)
    , offerDeadline      :: !(UIntN 256)
    , offerNonce         :: !(Maybe (UIntN 256))
    , offerSender        :: !Address
    , offerSignature     :: !Bytes
    }
  deriving (Eq, GHC.Generic)

instance Show Offer where
    show Offer{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' <$>
            [ C8.pack (show offerModel)
            , C8.pack (show offerObjective)
            , C8.pack (show offerToken)
            , C8.pack (show offerCost)
            , C8.pack (show offerValidator)
            , C8.pack (show offerLighthouse)
            , C8.pack (show offerLighthouseFee)
            , C8.pack (show offerDeadline)
            , C8.pack (show offerNonce)
            , C8.pack (show offerSender)
            , C8.pack (show (BA.convert offerSignature :: HexString))
            ]

instance Generic Offer
instance RobonomicsMsg Offer where
    hash = offerHash

instance FromJSON Offer where
    parseJSON = withObject "Offer" $ \v -> Offer
        <$> v .: "model"
        <*> v .: "objective"
        <*> v .: "token"
        <*> v .: "cost"
        <*> v .: "validator"
        <*> v .: "lighthouse"
        <*> v .: "lighthouseFee"
        <*> v .: "deadline"
        <*> pure Nothing
        <*> v .: "sender"
        <*> (b16decode =<< v .: "signature")

data Report = Report
  { reportLiability :: !Address
  , reportResult    :: !Base58String
  , reportSuccess   :: !Bool
  , reportSignature :: !Bytes
  } deriving (Eq, GHC.Generic)

instance Show Report where
    show Report{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' <$>
            [ C8.pack (show reportLiability)
            , C8.pack (show reportResult)
            , C8.pack (show reportSuccess)
            , C8.pack (show (BA.convert reportSignature :: HexString))
            ]

instance Generic Report
instance RobonomicsMsg Report where
    hash = reportHash

instance FromJSON Report where
    parseJSON = withObject "Report" $ \v -> Report
        <$> v .: "liability"
        <*> v .: "result"
        <*> v .: "success"
        <*> (b16decode =<< v .: "signature")

demandHash :: Demand -> Digest Keccak_256
{-# INLINE demandHash #-}
demandHash Demand{..} =
    Crypto.hash $ toBytes demandModel
               <> toBytes demandObjective
               <> BA.drop 12 (encode demandToken)
               <> encode demandCost
               <> BA.drop 12 (encode demandLighthouse)
               <> BA.drop 12 (encode demandValidator)
               <> encode demandValidatorFee
               <> encode demandDeadline
               <> encode (fromJust demandNonce)
               <> BA.drop 12 (encode demandSender)

offerHash :: Offer -> Digest Keccak_256
{-# INLINE offerHash #-}
offerHash Offer{..} =
    Crypto.hash $ toBytes offerModel
               <> toBytes offerObjective
               <> BA.drop 12 (encode offerToken)
               <> encode offerCost
               <> BA.drop 12 (encode offerValidator)
               <> BA.drop 12 (encode offerLighthouse)
               <> encode offerLighthouseFee
               <> encode offerDeadline
               <> encode (fromJust offerNonce)
               <> BA.drop 12 (encode offerSender)

reportHash :: Report -> Digest Keccak_256
{-# INLINE reportHash #-}
reportHash Report{..} =
    Crypto.hash $ BA.drop 12 (encode reportLiability)
               <> toBytes reportResult
               <> BA.drop 31 (encode reportSuccess)

b16decode :: Monad m => Text -> m Bytes
b16decode = either fail return . convertFromBase Base16 . encodeUtf8
