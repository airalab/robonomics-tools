{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Network.Robonomics.Lighthouse.MatcherStore where

import           Control.Monad              (when)
import           Control.Monad.State        (MonadState (..), modify')
import           Data.Base58String.Bitcoin  (Base58String, toBytes)
import           Data.Hashable              (Hashable (..))
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M (delete, foldl', insert,
                                                  lookup, size)
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as S (insert, member)
import           Data.Heap                  (MaxPrioHeap)
import qualified Data.Heap                  as H (insert, singleton, size, view)
import           Data.Solidity.Prim         (Address, Bytes, UIntN)
import           GHC.TypeLits

import           Network.Robonomics.Message (Demand (..), Offer (..))

instance (KnownNat n, n <= 256) => Hashable (UIntN n) where
    hash = hash . toInteger
    hashWithSalt x = hashWithSalt x . hash

instance Hashable Address where
    hash = hash . show
    hashWithSalt x = hashWithSalt x . hash

instance Hashable Bytes where
    hash = hash . show
    hashWithSalt x = hashWithSalt x . hash

instance Hashable Base58String where
    hash = hash . toBytes
    hashWithSalt x = hashWithSalt x . hash


data MatcherStore = MatcherStore
    { matcherDemandMap :: !(HashMap Int (MaxPrioHeap (UIntN 256) Demand))
    , matcherOfferMap  :: !(HashMap Int (MaxPrioHeap (UIntN 256) Offer))
    , matcherDemands   :: !(HashSet Demand)
    , matcherOffers    :: !(HashSet Offer)
    } deriving Show

demandKey :: Demand -> Int
demandKey Demand{..}
  = hash demandModel `hashWithSalt` demandObjective
                     `hashWithSalt` demandLighthouse
                     `hashWithSalt` demandToken
                     `hashWithSalt` demandCost

offerKey :: Offer -> Int
offerKey Offer{..}
  = hash offerModel `hashWithSalt` offerObjective
                    `hashWithSalt` offerLighthouse
                    `hashWithSalt` offerToken
                    `hashWithSalt` offerCost

storeSize :: MatcherStore -> (Int, Int)
storeSize MatcherStore{..} = (sizeOf matcherDemandMap, sizeOf matcherOfferMap)
  where sizeOf = M.foldl' (\s -> (s +) . H.size) 0

matchDemand :: MonadState MatcherStore m
            => Demand
            -> m (Maybe Offer)
matchDemand demand = do
    store <- get
    let key = demandKey demand
        offerMap = matcherOfferMap store
    case H.view =<< M.lookup key offerMap of
        Nothing -> do
            -- Store demand for future usage
            insertDemand demand
            return Nothing

        Just ((_, offer), offerHeap) -> do
            let update = M.insert key offerHeap
            -- Drop used offer from storage
            put (store { matcherOfferMap = update offerMap })
            return (Just offer)

insertDemand :: MonadState MatcherStore m
             => Demand
             -> m ()
insertDemand demand = modify' $ \matcherStore ->
    if not $ S.member demand $ matcherDemands matcherStore
        then
            let demandMap = matcherDemandMap matcherStore
                key = demandKey demand
                update = M.insert key $
                    case M.lookup key demandMap of
                        -- XXX: validator fee based priority
                        Nothing -> H.singleton (demandValidatorFee demand, demand)
                        Just h  -> H.insert (demandValidatorFee demand, demand) h
            in matcherStore { matcherDemandMap = update demandMap
                            , matcherDemands = S.insert demand (matcherDemands matcherStore)
                            }
        else matcherStore

matchOffer :: MonadState MatcherStore m
           => Offer
           -> m (Maybe Demand)
matchOffer offer = do
    store <- get
    let key = offerKey offer
        demandMap = matcherDemandMap store
    case H.view =<< M.lookup key demandMap of
        Nothing -> do
            -- Store demand for future usage
            insertOffer offer
            return Nothing

        Just ((_, demand), demandHeap) -> do
            let update = M.insert key demandHeap
            -- Drop used offer from storage
            put (store { matcherDemandMap = update demandMap })
            return (Just demand)

insertOffer :: MonadState MatcherStore m
            => Offer
            -> m ()
insertOffer offer = modify' $ \matcherStore ->
    if not $ S.member offer $ matcherOffers matcherStore
        then
            let offerMap = matcherOfferMap matcherStore
                key = offerKey offer
                update = M.insert key $
                    case M.lookup key offerMap of
                        -- XXX: lighthouse fee based priority
                        Nothing -> H.singleton (offerLighthouseFee offer, offer)
                        Just h  -> H.insert (offerLighthouseFee offer, offer) h
            in matcherStore { matcherOfferMap = update offerMap
                            , matcherOffers = S.insert offer (matcherOffers matcherStore)
                            }
        else matcherStore
