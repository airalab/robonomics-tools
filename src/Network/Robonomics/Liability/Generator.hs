{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Robonomics.Liability.Generator where
{-
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

mkDemand :: MonadRandom m => Address -> Address -> UIntN 256 -> m Demand
mkDemand lighthouse xrt deadline nonce sender =
    Demand <$> getRandomBytes 34
           <*> getRandomBytes 34
           <*> pure xrt
           <*> pure 1
           <*> pure lighthouse
           <*> pure "0x0000000000000000000000000000000000000000"
           <*> pure 0
           <*> pure deadline
           <*> pure nonce

           <*> pure ""

-- | Generate random demand/offer pair
randomDeal :: ( MonadLogger m
              , MonadRandom m
              )
           => Address
           -- ^ Lighthouse address
           -> Address
           -- ^ XRT address
           -> Quantity
           -- ^ Current block number
           -> PrivateKey
           -- ^ Ethereum private key
           -> m (Demand, Offer)
randomDeal lighthouse xrt block key = do
    let deadline = fromIntegral (block + 100)
    $logDebug $ "Estimated deadline: " <> T.pack (show deadline)

    demand@Demand{..} <- mkDemand lighthouse xrt deadline
    $logDebug $ "Generated Demand message: " <> T.pack (show demand)

    nonce <- randomNonce
    let offer = Offer demandModel
                      demandObjective
                      demandToken
                      demandCost
                      demandLighthouse
                      demandValidator
                      0
                      demandDeadline
                      nonce
                      mempty
    $logDebug $ "Associated Offer message: " <> T.pack (show offer)

    let signed = ( demand { demandSignature = sign key demand }
                 , offer { offerSignature = sign key offer } )
    $logInfo $ "The deal was generated: " <> T.pack (show signed)

    return signed

mkReport :: MonadRandom m => Address -> m Report
mkReport liability =
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
    rep <- mkReport liability
    $logDebug $ "Generated REPORT: " <> T.pack (show rep)

    let signed = rep { reportSignature = sign key rep }
    $logInfo $ "The report was generated: " <> T.pack (show signed)

    return signed
-}
