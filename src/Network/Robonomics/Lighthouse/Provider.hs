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
                                                              when)
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
                                                              receiptLogs)
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
import           Network.Robonomics.InfoChan                 (subscribe)
import           Network.Robonomics.InfoChan                 (Msg (MkReport))
import           Network.Robonomics.Liability                (Liability (..))
import qualified Network.Robonomics.Liability                as Liability (create,
                                                                           finalize,
                                                                           list,
                                                                           read)
import           Network.Robonomics.Liability.Generator      (randomDeal,
                                                              randomReport)
import           Network.Robonomics.Lighthouse.SimpleMatcher (matchOrders)

instance MonadRandom (LoggingT (ReaderT r Web3)) where
    getRandomBytes = liftIO . getRandomBytes

data Config = Config
    { web3Provider   :: !Provider
    , web3Account    :: !LocalKey
    , ipfsProvider   :: !String
    , lighthouseName :: !String
    , factoryName    :: !String
    , ens            :: !Address
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
        res <- web3 $ resolve ens $ C8.pack factoryName
        case res of
            Left _ -> do
                $logError $ "Unable to find factory with name " <> T.pack factoryName
                return ()

            Right factoryAddress -> forever $ do
                    Right nonce <- web3 $ withParam (to .~ factoryAddress) $ Factory.nonceOf accountAddress
                    $logInfo $ "Account nonce: "<> T.pack (show nonce)

                    deal <- randomDeal lighthouseAddress nonce key
                    Right receipt <- web3 $ Liability.create lighthouseAddress deal
                    let Right liabilityAddress = decode (changeTopics (receiptLogs receipt !! 2) !! 1)

                    report <- randomReport liabilityAddress key
                    web3 $ Liability.finalize lighthouseAddress report

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

        let asyncWeb3 = runWeb3' web3Provider . forkWeb3 . withAccount web3Account
            runSafe = flip catchAll ($logError . T.pack . show) . void

            create   = lift . runSafe . asyncWeb3 . Liability.create lighthouseAddress
            finalize = lift . runSafe . asyncWeb3 . Liability.finalize lighthouseAddress

            dispatchReport = forever $ do
                msg <- await
                case msg of
                  MkReport report -> finalize report
                  _               -> yield msg

            orders = subscribe ipfsProvider lighthouseName
                  >-> dispatchReport
                  >-> matchOrders

        runEffect $ for orders create

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
