{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Robonomics.Liability.Generator
    (
      randomDeal
    , randomReport
    ) where

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.Chan                (newChan, readChan,
                                                         writeChan)
import           Control.Monad                          (forever)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Logger                   (MonadLogger, logDebug,
                                                         logInfo)
import           Control.Monad.Trans                    (lift)
import           Crypto.Ethereum                        (PrivateKey,
                                                         derivePubKey)
import           Crypto.Random                          (MonadRandom (..))
import qualified Data.ByteArray                         as BA
import           Data.ByteArray.HexString               (HexString)
import           Data.ByteArray.Sized                   (unsafeFromByteArrayAccess)
import           Data.Default                           (def)
import           Data.Either                            (rights)
import           Data.Monoid                            (mempty)
import           Data.Solidity.Abi.Codec                (encode, encode')
import           Data.Solidity.Event                    (decodeEvent)
import           Data.Solidity.Prim.Address             (fromPubKey)
import qualified Data.Text                              as T
import qualified Network.Ethereum.Api.Eth               as Eth
import           Network.Ethereum.Api.Provider
import           Network.Ethereum.Api.Types             (Change,
                                                         DefaultBlock (..),
                                                         Filter (..), Quantity,
                                                         TxReceipt (..))
import           Network.Ethereum.Web3

import qualified Network.Robonomics.Contract.Factory    as F
import qualified Network.Robonomics.Contract.Lighthouse as L
import           Network.Robonomics.Message

randDemand :: MonadRandom m => Address -> UIntN 256 -> Address -> m Demand
randDemand lighthouse nonce sender =
    Demand <$> getRandomBytes 34
           <*> getRandomBytes 34
           <*> pure "0x0000000000000000000000000000000000000000"
           <*> pure 0
           <*> pure lighthouse
           <*> pure "0x0000000000000000000000000000000000000000"
           <*> pure 0
           <*> pure (2 ^ 100)
           <*> pure (Just nonce)
           <*> pure sender
           <*> pure mempty

pairOffer :: Demand -> Offer
pairOffer Demand{..} =
    Offer demandModel
          demandObjective
          demandToken
          demandCost
          demandValidator
          demandLighthouse
          0
          demandDeadline
          (fmap (+ 1) demandNonce)
          demandSender
          mempty

-- | Generate random demand/offer pair
randomDeal :: ( MonadLogger m
              , MonadRandom m
              )
           => Address
           -- ^ Lighthouse address
           -> UIntN 256
           -- ^ Current nonce value
           -> PrivateKey
           -- ^ Ethereum private key
           -> m (Demand, Offer)
randomDeal lighthouse nonce key = do
    let sender = fromPubKey (derivePubKey key)

    demand <- randDemand lighthouse nonce sender
    $logDebug $ "Generated Demand message: " <> T.pack (show demand)

    let offer = pairOffer demand
    $logDebug $ "Associated Offer message: " <> T.pack (show offer)

    let signed = ( demand { demandSignature = sign key demand, demandNonce = Nothing }
                 , offer { offerSignature = sign key offer, offerNonce = Nothing } )
    $logInfo $ "The deal was generated: " <> T.pack (show signed)

    return signed

randReport :: MonadRandom m => Address -> m Report
randReport liability =
    Report <$> pure liability
           <*> getRandomBytes 34
           <*> pure True
           <*> pure mempty

-- | Generate random report for given liability
randomReport :: (MonadRandom m, MonadLogger m)
             => Address
             -- ^ Liability address
             -> PrivateKey
             -- ^ Ethereum private key
             -> m Report
randomReport liability key = do
    rep <- randReport liability
    $logDebug $ "Generated REPORT: " <> T.pack (show rep)

    let signed = rep { reportSignature = sign key rep }
    $logInfo $ "The report was generated: " <> T.pack (show signed)

    return signed
