{-# LANGUAGE OverloadedStrings #-}
module Options where

import           Data.Solidity.Prim            (Address)
import           Network.Ethereum.Api.Provider (Provider (..))
import           Options.Applicative

data Options = Options
    { optionsProvider :: !Provider
    , optionsIpfs     :: !String
    , optionsFactory  :: !String
    , optionsEns      :: !Address
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> option (HttpProvider <$> str) (long "web3" <> value infura <> metavar "URI" <> help "Ethereum node endpoint [DEFAULT: Infura mainnet]")
    <*> option str (long "ipfs" <> value "/ip4/127.0.0.1/tcp/5001" <> metavar "MULTIADDR" <> help "IPFS node endpoint [DEFAULT: localhost]")
    <*> option str (long "factory" <> value "factory.5.robonomics.eth" <> metavar "ENS" <> help "Robonomics liability factory name")
    <*> option str (long "ens" <> value "0x314159265dD8dbb310642f98f50C066173C1259b" <> metavar "ADDRESS" <> help "ENS registry contract")

infura :: Provider
infura = HttpProvider "https://mainnet.infura.io/v3/1ba07380f3e740148f89852159695c73"
