{-# LANGUAGE FlexibleContexts  #-}
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
import           Control.Monad.IO.Class                      (MonadIO (..))
import           Control.Monad.Logger                        (MonadLogger,
                                                              logError, logInfo,
                                                              runStderrLoggingT)
import           Control.Monad.Trans                         (lift)
import           Control.Monad.Trans.Control                 (MonadBaseControl)
import           Crypto.Secp256k1                            (derivePubKey)
import           Data.ByteString                             (ByteString)
import qualified Data.ByteString.Char8                       as C8 (pack)
import           Data.Default                                (def)
import           Data.Machine
import           Data.Machine.Concurrent                     (mergeSum, (>~>))
import           Data.Solidity.Prim.Address                  (fromPubKey)
import qualified Data.Text                                   as T
import           Lens.Micro                                  ((.~))
import           Network.Ethereum.Account                    (PrivateKeyAccount)
import qualified Network.Ethereum.Api.Eth                    as Eth
import           Network.Ethereum.Api.Provider               (Provider,
                                                              forkWeb3,
                                                              runWeb3')
import           Network.Ethereum.Api.Types                  (DefaultBlock (Latest))
import           Network.Ethereum.Ens                        (namehash)
import qualified Network.Ethereum.Ens.PublicResolver         as Resolver
import qualified Network.Ethereum.Ens.Registry               as Reg
import           Network.Ethereum.Web3
import           Network.JsonRpc.TinyClient                  (JsonRpc)

import qualified Network.Robonomics.Contract.Lighthouse      as Lighthouse
import qualified Network.Robonomics.Contract.XRT             as XRT
import           Network.Robonomics.InfoChan                 (subscribe)
import           Network.Robonomics.Liability                (Liability (..))
import qualified Network.Robonomics.Liability                as Liability (create,
                                                                           finalize,
                                                                           list,
                                                                           read)
import           Network.Robonomics.Liability.Generator      (randomDeal,
                                                              randomReport)
import           Network.Robonomics.Lighthouse.SimpleMatcher (match)

data Config = Config
    { web3Provider   :: !Provider
    , web3Account    :: !PrivateKey
    , ipfsProvider   :: !String
    , lighthouseName :: !String
    , ens            :: !Address
    } deriving (Eq, Show)

{-
selfProvider :: (MonadIO m, MonadLogger m) => ProviderConfig -> m ()
selfProvider ProviderConfig{..} = do
    web3 $ withParam (to .~ xrtAddress) $ do
        allowance <- XRT.allowance accountAddress factoryAddress
        when (allowance == 0) $ do
            XRT.approve factoryAddress 1000
            return ()

    liabilityChan <- liftIO newChan

    runWeb3' web3Provider $ forkWeb3 $
        Liability.list factoryAddress Latest Latest $ \_ liabilityAddress -> do
            liability <- withAccount web3Account $ Liability.read liabilityAddress
            liftIO $ writeChan liabilityChan (liabilityAddress, liability)

    forever $ do
        Right block <- runWeb3' web3Provider Eth.blockNumber
        deal <- randomDeal lighthouseAddress xrtAddress block key
        web3 $ Liability.create lighthouseAddress deal
        selfReport liabilityChan
  where
    web3 = runWeb3' web3Provider . withAccount web3Account
    PrivateKey key _ = web3Account
    accountAddress = fromPubKey (derivePubKey key)
    selfReport chan = do
        (address, Liability{..}) <- liftIO $ readChan chan
        if liabilityPromisor == accountAddress
        then do
            report <- randomReport address key
            web3 $ Liability.finalize lighthouseAddress report
        else selfReport chan
-}

ipfs :: ( MonadBaseControl IO m
        , MonadIO m
        , MonadLogger m
        , MonadCatch m
        )
     => Config
     -> m ()
ipfs Config{..} = do
    let PrivateKey key _ = web3Account
        accountAddress = fromPubKey (derivePubKey key)
        web3 = runWeb3' web3Provider . withAccount web3Account

    $logInfo "Starting Robonomics provider..."
    $logInfo $ "Account address: " <> T.pack (show accountAddress)


    res <- web3 $ resolve ens $ C8.pack lighthouseName
    case res of
        Left _ -> do
            $logError $ "Unable to find lighthouse with name " <> T.pack lighthouseName
            return ()

        Right lighthouseAddress -> do
            $logInfo $ "Lighthouse found, name: " <> T.pack lighthouseName <> ", address: " <> T.pack (show lighthouseAddress)

            let web3Safe = flip catchAll ($logError . T.pack . show) . void . web3

            web3Safe $ withParam (to .~ lighthouseAddress) $ do
                balance <- Lighthouse.balances accountAddress
                minBalance <- Lighthouse.minimalFreeze
                when (balance < minBalance) $ do
                    xrt <- Lighthouse.xrt
                    withParam (to .~ xrt) $
                        XRT.approve lighthouseAddress minBalance
                    void $ Lighthouse.refill minBalance

            liftIO $ async $ runStderrLoggingT $ do
                Right xrt <- runWeb3' web3Provider $ withAccount web3Account $
                        withParam (to .~ lighthouseAddress) Lighthouse.xrt
                forever $ do
                    Right (xrt, eth) <- runWeb3' web3Provider $ do
                        balanceEth <- Eth.getBalance accountAddress Latest
                        balance <- withAccount web3Account $ withParam (to .~ xrt) $
                            XRT.balanceOf accountAddress
                        return (fromIntegral balance / 10^9, fromWei balanceEth)

                    $logInfo $ "BALANCE " <> T.pack (show xrt) <> " XRT " <> T.pack (show (eth :: Ether))

                    liftIO $ threadDelay 60000000

            runT_ $ subscribe ipfsProvider lighthouseName
                >~> match
                >~> mergeSum (autoM $ web3Safe . Liability.finalize lighthouseAddress)
                             (autoM $ web3Safe . Liability.create lighthouseAddress)

-- | Get address of ENS domain
resolve :: JsonRpc m
        => Address
        -- ^ Registry address
        -> ByteString
        -- ^ Domain name
        -> PrivateKeyAccount m Address
        -- ^ Associated address
resolve reg name = do
    r <- ensRegistry $ Reg.resolver node
    withParam (to .~ r) $ Resolver.addr node
  where
    node = namehash name
    ensRegistry = withParam $ to .~ reg
