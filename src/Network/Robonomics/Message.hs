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
  , Accepted(..)
  , Pending(..)
  ) where

import           Crypto.Ethereum           (PrivateKey, signMessage)
import           Crypto.Hash               (Digest, Keccak_256)
import qualified Crypto.Hash               as Crypto
import           Data.Aeson                (FromJSON (..), ToJSON (..), object,
                                            withObject, (.:), (.=))
import           Data.Base58String.Bitcoin (Base58String, toBytes)
import           Data.ByteArray            (ByteArray, ByteArrayAccess)
import qualified Data.ByteArray            as BA (convert, drop)
import           Data.ByteArray.Encoding   (Base (Base16), convertFromBase)
import           Data.ByteArray.HexString  (HexString)
import qualified Data.ByteArray.HexString  as HS (toBytes)
import           Data.ByteArray.Sized      (unsafeSizedByteArray)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C8 (concat, cons, pack, unpack)
import qualified Data.ByteString.Short     as Short (fromShort, pack)
import           Data.Hashable             (Hashable (hashWithSalt))
import           Data.Monoid               ((<>))
import           Data.Proxy                (Proxy (..))
import           Data.Solidity.Abi         (AbiPut (..), AbiType (..))
import           Data.Solidity.Abi.Codec   (encode)
import           Data.Solidity.Prim        (Address, Bytes, BytesN, UIntN)
import           Data.Text                 (Text)
import           Data.Text.Encoding        (encodeUtf8)
import           Generics.SOP              (Generic)
import qualified GHC.Generics              as GHC (Generic)
import           GHC.TypeLits

instance (KnownNat n, n <= 256) => FromJSON (UIntN n) where
    parseJSON = fmap fromInteger . parseJSON

instance (KnownNat n, n <= 256) => ToJSON (UIntN n) where
    toJSON = toJSON . toInteger

class RobonomicsMsg a where
    hash :: a -> Digest Keccak_256

    sign :: ByteArray ba => PrivateKey -> a -> ba
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
    , demandNonce        :: !(UIntN 256)
    , demandSender       :: !Address
    , demandSignature    :: !HexString
    }
  deriving Eq

instance Show Demand where
    show Demand{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' . C8.pack <$>
            [ show demandModel
            , show demandObjective
            , show demandToken
            , show demandCost
            , show demandLighthouse
            , show demandValidator
            , show demandValidatorFee
            , show demandDeadline
            , show demandNonce
            , show demandSender
            , show demandSignature
            ]

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
        <*> v .: "nonce"
        <*> v .: "sender"
        <*> v .: "signature"

instance ToJSON Demand where
    toJSON Demand{..} = object
        [ "model"        .= demandModel
        , "objective"    .= demandObjective
        , "token"        .= demandToken
        , "cost"         .= demandCost
        , "lighthouse"   .= demandLighthouse
        , "validator"    .= demandValidator
        , "validatorFee" .= demandValidatorFee
        , "deadline"     .= demandDeadline
        , "nonce"        .= demandNonce
        , "sender"       .= demandSender
        , "signature"    .= demandSignature
        ]

instance AbiType Demand where
    isDynamic = const False

instance AbiPut Demand where
    abiPut Demand{..} = abiPut
        ( toBytes demandModel
        , toBytes demandObjective
        , demandToken
        , demandCost
        , demandLighthouse
        , demandValidator
        , demandValidatorFee
        , demandDeadline
        , demandSender
        , (HS.toBytes demandSignature :: Bytes)
        )

instance Hashable Demand where
    hashWithSalt = flip (flip hashWithSalt . bs . demandHash)
      where bs = BA.convert :: ByteArrayAccess ba => ba -> ByteString

data Offer = Offer
    { offerModel         :: !Base58String
    , offerObjective     :: !Base58String
    , offerToken         :: !Address
    , offerCost          :: !(UIntN 256)
    , offerValidator     :: !Address
    , offerLighthouse    :: !Address
    , offerLighthouseFee :: !(UIntN 256)
    , offerDeadline      :: !(UIntN 256)
    , offerNonce         :: !(UIntN 256)
    , offerSender        :: !Address
    , offerSignature     :: !HexString
    }
  deriving Eq

instance Show Offer where
    show Offer{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' . C8.pack <$>
            [ show offerModel
            , show offerObjective
            , show offerToken
            , show offerCost
            , show offerValidator
            , show offerLighthouse
            , show offerLighthouseFee
            , show offerDeadline
            , show offerNonce
            , show offerSender
            , show offerSignature
            ]

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
        <*> v .: "nonce"
        <*> v .: "sender"
        <*> v .: "signature"

instance ToJSON Offer where
    toJSON Offer{..} = object
        [ "model"         .= offerModel
        , "objective"     .= offerObjective
        , "token"         .= offerToken
        , "cost"          .= offerCost
        , "validator"     .= offerValidator
        , "lighthouse"    .= offerLighthouse
        , "lighthouseFee" .= offerLighthouseFee
        , "deadline"      .= offerDeadline
        , "nonce"         .= offerNonce
        , "sender"        .= offerSender
        , "signature"     .= offerSignature
        ]

instance AbiType Offer where
    isDynamic = const False

instance AbiPut Offer where
    abiPut Offer{..} = abiPut
        ( toBytes offerModel
        , toBytes offerObjective
        , offerToken
        , offerCost
        , offerValidator
        , offerLighthouse
        , offerLighthouseFee
        , offerDeadline
        , offerSender
        , (HS.toBytes offerSignature :: Bytes)
        )

instance Hashable Offer where
    hashWithSalt = flip (flip hashWithSalt . bs . offerHash)
      where bs = BA.convert :: ByteArrayAccess ba => ba -> ByteString

data Report = Report
  { reportLiability :: !Address
  , reportResult    :: !Base58String
  , reportSuccess   :: !Bool
  , reportSignature :: !HexString
  } deriving (Eq, GHC.Generic)

instance Show Report where
    show Report{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' <$>
            [ C8.pack (show reportLiability)
            , C8.pack (show reportResult)
            , C8.pack (show reportSuccess)
            , C8.pack (show reportSignature)
            ]

instance Generic Report
instance RobonomicsMsg Report where
    hash = reportHash

instance FromJSON Report where
    parseJSON = withObject "Report" $ \v -> Report
        <$> v .: "liability"
        <*> v .: "result"
        <*> v .: "success"
        <*> v .: "signature"

instance ToJSON Report where
    toJSON Report{..} = object
        [ "liability" .= reportLiability
        , "result" .= reportResult
        , "success" .= reportSuccess
        , "signature" .= reportSignature
        ]

instance Hashable Report where
    hashWithSalt = flip (flip hashWithSalt . bs . reportHash)
      where bs = BA.convert :: ByteArrayAccess ba => ba -> ByteString

data Accepted = Accepted
  { acceptedOrderHash :: !HexString
  , acceptedTime      :: !(UIntN 256)
  , acceptedSignature :: !HexString
  } deriving (Eq, GHC.Generic)

instance Show Accepted where
    show Accepted{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' <$>
            [ C8.pack (show acceptedOrderHash)
            , C8.pack (show acceptedTime)
            , C8.pack (show acceptedSignature)
            ]

instance Generic Accepted
instance RobonomicsMsg Accepted where
    hash = acceptedHash

instance FromJSON Accepted where
    parseJSON = withObject "Accepted" $ \v -> Accepted
        <$> v .: "order"
        <*> v .: "accepted"
        <*> v .: "signature"

instance ToJSON Accepted where
    toJSON Accepted{..} = object
        [ "order" .= acceptedOrderHash
        , "accepted" .= acceptedTime
        , "signature" .= acceptedSignature
        ]

instance Hashable Accepted where
    hashWithSalt = flip (flip hashWithSalt . bs . acceptedHash)
      where bs = BA.convert :: ByteArrayAccess ba => ba -> ByteString

newtype Pending = Pending
  { pendingTransaction :: HexString
  } deriving(Eq, GHC.Generic)

instance Show Pending where
    show = show . pendingTransaction

instance Generic Pending
instance FromJSON Pending where
    parseJSON = withObject "Pending" $ \v -> Pending
        <$> v .: "tx"

instance ToJSON Pending where
    toJSON Pending{..} = object
        [ "tx" .= pendingTransaction ]

instance Hashable Pending where
    hashWithSalt = flip (flip hashWithSalt . bs . pendingHash)
      where bs = BA.convert :: ByteArrayAccess ba => ba -> ByteString

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
               <> encode demandNonce
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
               <> encode offerNonce
               <> BA.drop 12 (encode offerSender)

reportHash :: Report -> Digest Keccak_256
{-# INLINE reportHash #-}
reportHash Report{..} =
    Crypto.hash $ BA.drop 12 (encode reportLiability)
               <> toBytes reportResult
               <> BA.drop 31 (encode reportSuccess)

acceptedHash :: Accepted -> Digest Keccak_256
{-# INLINE acceptedHash #-}
acceptedHash Accepted{..} =
    Crypto.hash $ acceptedOrderHash
               <> encode acceptedTime

pendingHash :: Pending -> Digest Keccak_256
{-# INLINE pendingHash #-}
pendingHash = Crypto.hash . pendingTransaction
