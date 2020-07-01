{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Default                  (def)
import           Network.Web3.Provider
import           Network.Ethereum
import           Options.Applicative
import           System.Exit

import qualified Network.Robonomics.Liability  as L

import           Options

infura :: Provider
infura = HttpProvider "https://mainnet.infura.io/v3/1ba07380f3e740148f89852159695c73"

main :: IO ()
main = run =<< execParser opts
  where
    opts = info
        (options <**> helper)
        (fullDesc <> progDesc "Robonomics liability smart contract ops")

run :: Options -> IO ()
run (Options cmd) =
    case cmd of
        Read a -> do
            r <- runWeb3' infura $ withAccount def $ L.read a
            case r of
                Right l -> do
                    print l
                    exitSuccess
                Left e -> do
                    print e
                    exitFailure
        List factory from to -> do
            r <- runWeb3' infura $ L.list factory from to $ \b a ->
                liftIO . putStrLn $ show b ++ " " ++ show a
            case r of
                Right _ -> exitSuccess
                Left e -> do
                    print e
                    exitFailure
