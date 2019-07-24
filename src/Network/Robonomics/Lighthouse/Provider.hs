{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Robonomics.Lighthouse.Provider where

import           Control.Concurrent                          (newChan, readChan,
                                                              threadDelay,
                                                              writeChan)
import           Control.Concurrent.Async                    (async)
import           Control.Monad                               (forever, void,
                                                              when, (<=<))
import           Control.Monad.Catch                         (MonadCatch,
                                                              catchAll)
import           Control.Monad.Fail                          (MonadFail)
import           Control.Monad.IO.Class                      (MonadIO (..))
import           Control.Monad.Logger                        (LoggingT,
                                                              MonadLogger,
                                                              logError, logInfo,
                                                              runChanLoggingT,
                                                              runStderrLoggingT,
                                                              unChanLoggingT)
import           Control.Monad.Reader                        (ReaderT)
import           Control.Monad.Trans                         (lift)
import           Control.Monad.Trans.Control                 (MonadBaseControl)
import           Crypto.Ethereum                             (PrivateKey)
import           Crypto.Ethereum.Utils                       (derivePubKey)
import           Crypto.Random                               (MonadRandom (..))
import qualified Data.ByteArray                              as BA (convert)
import           Data.ByteString                             (ByteString)
import qualified Data.ByteString.Char8                       as C8 (pack)
import           Data.Default                                (def)
import           Data.Solidity.Abi.Codec                     (decode)
import           Data.Solidity.Prim.Address                  (fromPubKey)
import qualified Data.Text                                   as T
import           Lens.Micro                                  ((.~))
import           Network.Ethereum.Account                    (LocalKey (..),
                                                              LocalKeyAccount)
import qualified Network.Ethereum.Api.Eth                    as Eth
import           Network.Ethereum.Api.Provider               (Provider,
                                                              Web3Error,
                                                              forkWeb3,
                                                              runWeb3')
import           Network.Ethereum.Api.Types                  (DefaultBlock (Latest),
                                                              Filter (..),
                                                              changeTopics,
                                                              receiptLogs,
                                                              receiptTransactionHash)
import           Network.Ethereum.Ens                        (namehash)
import qualified Network.Ethereum.Ens.PublicResolver         as Resolver
import qualified Network.Ethereum.Ens.Registry               as Reg
import           Network.Ethereum.Web3
import           Network.JsonRpc.TinyClient                  (JsonRpc)
import           Pipes                                       (await, for,
                                                              runEffect, yield,
                                                              (>->))

import qualified Network.Robonomics.Contract.Factory         as Factory
import qualified Network.Robonomics.Contract.Lighthouse      as Lighthouse
import qualified Network.Robonomics.Contract.XRT             as XRT
import           Network.Robonomics.InfoChan                 (Msg (..), publish,
                                                              subscribe)
import           Network.Robonomics.Liability                (Liability (..))
import qualified Network.Robonomics.Liability                as Liability (create,
                                                                           finalize,
                                                                           list,
                                                                           read)
import           Network.Robonomics.Liability.Generator      (randomDeal,
                                                              randomReport)
import           Network.Robonomics.Lighthouse.SimpleMatcher (matcher)
import           Network.Robonomics.Message                  (Accepted (..),
                                                              Pending (..),
                                                              Report (..),
                                                              RobonomicsMsg (..))

instance MonadRandom (LoggingT (ReaderT r Web3)) where
    getRandomBytes = liftIO . getRandomBytes

data Config = Config
    { web3Provider   :: !Provider
    , web3Account    :: !LocalKey
    , ipfsProvider   :: !String
    , lighthouseName :: !String
    , factoryName    :: !String
    , ens            :: !Address
    , gasprice       :: !Szabo
    } deriving (Eq, Show)

local :: ( MonadIO m
         , MonadFail m
         , MonadLogger m
         , MonadRandom m
         )
      => Config
      -> m ()
local cfg@Config{..} =
    connectLighthouse cfg $ \key accountAddress lighthouseAddress -> do
        $logInfo "Starting local miner..."
        $logInfo $ "Account address: " <> T.pack (show accountAddress)

        let web3 = runWeb3' web3Provider . withAccount web3Account
            create = Liability.create lighthouseAddress gasprice
            finalize = Liability.finalize lighthouseAddress gasprice

        res <- web3 $ resolve ens $ C8.pack factoryName
        case res of
            Left _ -> do
                $logError $ "Unable to find factory with name " <> T.pack factoryName
                return ()

            Right factoryAddress -> forever $ do
                    Right nonce <- web3 $ withParam (to .~ factoryAddress) $ Factory.nonceOf accountAddress
                    $logInfo $ "Account nonce: "<> T.pack (show nonce)

                    deal <- randomDeal lighthouseAddress nonce key
                    Right receipt <- web3 $ create deal
                    let Right liabilityAddress = decode (changeTopics (receiptLogs receipt !! 2) !! 1)

                    report <- randomReport liabilityAddress key
                    web3 $ finalize report

ipfs :: ( MonadBaseControl IO m
        , MonadIO m
        , MonadLogger m
        , MonadCatch m
        )
     => Config
     -> m ()
ipfs cfg@Config{..} =
    connectLighthouse cfg $ \_ accountAddress lighthouseAddress -> do
        $logInfo "Starting IPFS provider..."
        $logInfo $ "Account address: " <> T.pack (show accountAddress)

        let web3 = runWeb3' web3Provider . withAccount web3Account
            runSafe = flip catchAll (const (return (Left undefined)) <=< $logError . T.pack . show)

            create   = lift . runSafe . web3 . fmap Just . Liability.create lighthouseAddress gasprice
            finalize = lift . runSafe . web3 . fmap Just . Liability.finalize lighthouseAddress gasprice

            dispatcher = forever $ do
                msg <- await
                case msg of
                    Right deal@(demand, offer) -> do
                        mbtx <- create deal
                        case mbtx of
                            Right (Just receipt) ->
                                let txHash = BA.convert (receiptTransactionHash receipt)
                                 in yield . MkPending $ Pending txHash
                            _ -> return ()

                    Left (MkReport report@Report{..}) -> do
                        mbtx <- finalize report
                        case mbtx of
                            Right (Just receipt) ->
                                let txHash = BA.convert (receiptTransactionHash receipt)
                                 in yield . MkPending $ Pending txHash
                            _ -> return ()

                    Left (MkDemand demand) ->
                        let feedback = Accepted (BA.convert $ hash demand) 0 ""
                         in yield . MkAccepted $ feedback { acceptedSignature = sign key feedback }

                    Left (MkOffer offer) ->
                        let feedback = Accepted (BA.convert $ hash offer) 0 ""
                         in yield . MkAccepted $ feedback { acceptedSignature = sign key feedback }

                    _ -> return ()

            pipeline = subscribe ipfsProvider lighthouseName
                   >-> matcher
                   >-> dispatcher
                   >-> publish ipfsProvider lighthouseName

        runEffect pipeline
  where LocalKey key _ = web3Account

connectLighthouse :: (MonadIO m, MonadLogger m)
                  => Config
                  -> (PrivateKey -> Address -> Address -> m ())
                  -> m ()
connectLighthouse cfg@Config{..} ma = do
    res <- web3 $ resolve ens $ C8.pack lighthouseName
    case res of
        Left _ -> do
            $logError $ "Unable to find lighthouse with name " <> T.pack lighthouseName
            return ()

        Right lighthouseAddress -> do
            $logInfo $ "Lighthouse found, name: " <> T.pack lighthouseName
                                                  <> ", address: " <> T.pack (show lighthouseAddress)

            liftIO $ async $ runStderrLoggingT $ do
                let xrtName = "xrt" ++ drop 11 (dropWhile (/= '.') lighthouseName)
                Right xrtAddress <- runWeb3' web3Provider $
                    withAccount web3Account $
                        resolve ens $ C8.pack xrtName
                forever $ do
                    Right (xrt, eth) <- runWeb3' web3Provider $ do
                        balanceEth <- Eth.getBalance accountAddress Latest
                        balance <- withAccount web3Account $ withParam (to .~ xrtAddress) $
                            XRT.balanceOf accountAddress
                        return (fromIntegral balance / 10^9, fromWei balanceEth)

                    $logInfo $ "BALANCE " <> T.pack (show xrt) <> " XRT " <> T.pack (show (eth :: Ether))

                    liftIO $ threadDelay 60000000
            ma key accountAddress lighthouseAddress
  where
    LocalKey key _ = web3Account
    accountAddress = fromPubKey (derivePubKey key)
    web3 = runWeb3' web3Provider . withAccount web3Account

-- | Get address of ENS domain
resolve :: JsonRpc m
        => Address
        -- ^ Registry address
        -> ByteString
        -- ^ Domain name
        -> LocalKeyAccount m Address
        -- ^ Associated address
resolve reg name = do
    r <- ensRegistry $ Reg.resolver node
    withParam (to .~ r) $ Resolver.addr node
  where
    node = namehash name
    ensRegistry = withParam $ to .~ reg
