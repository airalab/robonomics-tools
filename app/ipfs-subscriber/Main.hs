{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Concurrent                     (forkIO)
import           Control.Concurrent.Chan                (newChan, readChan,
                                                         writeChan)
import           Control.Monad                          (forM_, forever, void)
import           Control.Monad.Catch                    (catchAll)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Logger                   (LoggingT, logError,
                                                         logInfo,
                                                         runStderrLoggingT)
import           Control.Monad.Trans                    (lift)
import qualified Data.ByteString.Char8                  as C8
import           Data.Default                           (Default (..))
import qualified Data.Text                              as T (pack, unpack)
import           Network.Web3.Provider          (runWeb3')
import           Network.Ethereum.Api.Types             (DefaultBlock (..),
                                                         Filter (..))
import           Network.Ethereum
import           Options.Applicative
import           Pipes                                  (for, runEffect)
import           System.Exit                            (exitFailure)

import qualified Network.Ipfs.PubSub                    as PubSub (subscribe)
import qualified Network.Robonomics.Contract.Factory    as F
import           Network.Robonomics.Lighthouse.Provider (resolve)
import           Options

main :: IO ()
main = run =<< execParser opts
  where
    opts = info
        (options <**> helper)
        (fullDesc <> progDesc "ipfs-subscriber :: Robonomics IPFS PubSub subscriber")

run :: Options -> IO ()
run Options{..} = runStderrLoggingT $ do
    $logInfo "Robonomics IPFS subscriber launch"

    let web3 = runWeb3' optionsProvider . withAccount def
        domain = dropWhile (/= '.') optionsFactory
        subscriptions = (++ domain) <$> [ "graph" ]

    forM_ subscriptions $ \topic -> do
        liftIO $ subscribe optionsIpfs topic
        $logInfo $ "Subsription started for " <> T.pack topic

    res <- web3 $ resolve optionsEns $ C8.pack optionsFactory
    case res of
        Left _ -> do
            $logError $ "Unable to resolve factory contract: " <> T.pack optionsFactory
            liftIO exitFailure

        Right factory -> do
            let flt = (def :: Filter F.NewLighthouse)
                    { filterAddress   = Just [ factory ]
                    , filterFromBlock = optionsFromBlock
                    }

            chan <- liftIO newChan
            runWeb3' optionsProvider $ do
                -- Lighthouses in the past
                event flt $ \(F.NewLighthouse _ name) -> do
                    liftIO $ writeChan chan $ T.unpack name ++ ".lighthouse" ++ domain
                    return ContinueEvent
                -- Lighthouses in the future
                event (flt { filterFromBlock = Latest }) $ \(F.NewLighthouse _ name) -> do
                    liftIO $ writeChan chan $ T.unpack name ++ ".lighthouse" ++ domain
                    return ContinueEvent

            $logInfo $ "Lookup lighthouses of " <> T.pack optionsFactory
            forever $ do
                lighthouse <- liftIO $ readChan chan
                $logInfo $ "Lighthouse found: " <> T.pack lighthouse

                liftIO $ subscribe optionsIpfs lighthouse
                $logInfo $ "Subsription started for " <> T.pack lighthouse

subscribe :: String -> String -> IO ()
subscribe api topic = void $ liftIO $ forkIO $ forever $
    flip catchAll print $
        runEffect $ for (PubSub.subscribe api topic) $ void . pure
