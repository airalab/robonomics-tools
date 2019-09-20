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
import           Data.ByteArray            (ByteArrayAccess)
import qualified Data.ByteArray            as BA (convert, drop)
import           Data.ByteArray.Encoding   (Base (Base16), convertFromBase)
import           Data.ByteArray.HexString  (HexString, fromBytes)
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
    , demandNonce        :: !(UIntN 256)
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
        <*> v .: "nonce"
        <*> v .: "sender"
        <*> (b16decode =<< v .: "signature")

instance ToJSON Demand where
    toJSON Demand{..} = object
        [ "model" .= demandModel
        , "objective" .= demandObjective
        , "token" .= demandToken
        , "cost" .= demandCost
        , "lighthouse" .= demandLighthouse
        , "validator" .= demandValidator
        , "validatorFee" .= demandValidatorFee
        , "deadline" .= demandDeadline
        , "nonce" .= demandNonce
        , "sender" .= demandSender
        , "signature" .= (BA.convert demandSignature :: HexString)
        ]

instance AbiType Demand where
    isDynamic _ = False

instance AbiPut Demand where
    abiPut Demand{..} = do
        abiPut demandModel
        abiPut demandObjective
        abiPut demandToken
        abiPut demandCost
        abiPut demandLighthouse
        abiPut demandValidator
        abiPut demandValidatorFee
        abiPut demandDeadline
        abiPut demandSender
        abiPut demandSignature

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
        <*> v .: "nonce"
        <*> v .: "sender"
        <*> (b16decode =<< v .: "signature")

instance ToJSON Offer where
    toJSON Offer{..} = object
        [ "model" .= offerModel
        , "objective" .= offerObjective
        , "token" .= offerToken
        , "cost" .= offerCost
        , "validator" .= offerValidator
        , "lighthouse" .= offerLighthouse
        , "lighthouseFee" .= offerLighthouseFee
        , "deadline" .= offerDeadline
        , "nonce" .= offerNonce
        , "sender" .= offerSender
        , "signature" .= (BA.convert offerSignature :: HexString)
        ]

instance AbiType Offer where
    isDynamic _ = False

instance AbiPut Offer where
    abiPut Offer{..} = do
        abiPut offerModel
        abiPut offerObjective
        abiPut offerToken
        abiPut offerCost
        abiPut offerValidator
        abiPut offerLighthouse
        abiPut offerLighthouseFee
        abiPut offerDeadline
        abiPut offerSender
        abiPut offerSignature

instance Hashable Offer where
    hashWithSalt = flip (flip hashWithSalt . bs . offerHash)
      where bs = BA.convert :: ByteArrayAccess ba => ba -> ByteString

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

instance ToJSON Report where
    toJSON Report{..} = object
        [ "liability" .= reportLiability
        , "result" .= reportResult
        , "success" .= reportSuccess
        , "signature" .= (BA.convert reportSignature :: HexString)
        ]

instance Hashable Report where
    hashWithSalt = flip (flip hashWithSalt . bs . reportHash)
      where bs = BA.convert :: ByteArrayAccess ba => ba -> ByteString

data Accepted = Accepted
  { acceptedOrderHash :: !Bytes
  , acceptedTime      :: !(UIntN 256)
  , acceptedSignature :: !Bytes
  } deriving (Eq, GHC.Generic)

instance Show Accepted where
    show Accepted{..} =
        C8.unpack $ C8.concat $ C8.cons ' ' <$>
            [ C8.pack (show acceptedOrderHash)
            , C8.pack (show acceptedTime)
            , C8.pack (show (BA.convert acceptedSignature :: HexString))
            ]

instance Generic Accepted
instance RobonomicsMsg Accepted where
    hash = acceptedHash

instance FromJSON Accepted where
    parseJSON = withObject "Accepted" $ \v -> Accepted
        <$> v .: "order"
        <*> v .: "accepted"
        <*> (b16decode =<< v .: "signature")

instance ToJSON Accepted where
    toJSON Accepted{..} = object
        [ "order" .= acceptedOrderHash
        , "accepted" .= acceptedTime
        , "signature" .= (BA.convert acceptedSignature :: HexString)
        ]

instance Hashable Accepted where
    hashWithSalt = flip (flip hashWithSalt . bs . acceptedHash)
      where bs = BA.convert :: ByteArrayAccess ba => ba -> ByteString

newtype Pending = Pending
  { pendingTransaction :: Bytes
  } deriving(Eq, GHC.Generic)

instance Show Pending where
    show Pending{..} = show (BA.convert pendingTransaction :: HexString)

instance Generic Pending
instance FromJSON Pending where
    parseJSON = withObject "Pending" $ \v -> Pending
        <$> (b16decode =<< v .: "tx")

instance ToJSON Pending where
    toJSON Pending{..} = object
        [ "tx" .= (BA.convert pendingTransaction :: HexString) ]

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
pendingHash Pending{..} = Crypto.hash pendingTransaction

b16decode :: Monad m => Text -> m Bytes
b16decode = either fail return . convertFromBase Base16 . encodeUtf8
