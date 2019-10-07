{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Fee priority based order matcher.
--
module Network.Robonomics.Lighthouse.Matcher where

import           Control.Monad                              (forever)
import           Control.Monad.Logger                       (MonadLogger,
                                                             logDebug, logInfo)
import           Control.Monad.State                        (MonadState, gets)
import           Control.Monad.Trans                        (lift)
import qualified Data.Text                                  as T (pack)
import           Pipes                                      (Pipe, await, yield)

import           Network.Robonomics.InfoChan                (Msg (..))
import           Network.Robonomics.Lighthouse.MatcherStore (MatcherStore,
                                                             matchDemand,
                                                             matchOffer,
                                                             storeSize)
import           Network.Robonomics.Message                 (Demand (..),
                                                             Offer (..), Report)
type MatchResponse = Either Msg (Demand, Offer)

matcher :: ( MonadState MatcherStore m
           , MonadLogger m
           )
        => Pipe Msg MatchResponse m ()
matcher = forever $ do
    lift $ do
        size <- gets storeSize
        $logInfo $ "Holded messages (ASK's, BID's): " <> T.pack (show size)

    msg <- await
    lift $ $logInfo $ "Received message: " <> T.pack (show msg)

    case msg of
        MkDemand demand -> do
            mbOffer <- lift $ matchDemand demand
            case mbOffer of
                Nothing -> do
                    lift $ $logInfo $ "Not matched: " <> T.pack (show demand)
                    yield $ Left msg
                Just offer -> do
                    lift $ $logInfo $ "Matched: " <> T.pack (show (demand, offer))
                    yield $ Right (demand, offer)

        MkOffer offer -> do
            mbDemand <- lift $ matchOffer offer
            case mbDemand of
                Nothing -> do
                    lift $ $logInfo $ "Not matched: " <> T.pack (show offer)
                    yield $ Left msg
                Just demand -> do
                    lift $ $logInfo $ "Matched: " <> T.pack (show (demand, offer))
                    yield $ Right (demand, offer)

        _ -> yield (Left msg)
