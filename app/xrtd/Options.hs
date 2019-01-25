{-# LANGUAGE OverloadedStrings #-}
module Options where

import           Crypto.Ethereum                        (PrivateKey)
import           Crypto.Ethereum.Utils                  (importKey)
import           Data.ByteArray.HexString               (HexString)
import           Network.Ethereum.Account.LocalKey      (LocalKey (..))
import           Network.Ethereum.Api.Provider          (Provider (..))
import           Network.Ethereum.Chain
import           Options.Applicative

import qualified Network.Robonomics.Lighthouse.Provider as P

newtype Options = Options { providerConfig :: P.Config }

options :: Parser Options
options = Options <$> providerOptions

providerOptions :: Parser P.Config
providerOptions = P.Config
    <$> option (HttpProvider <$> str) (long "web3" <> value infura <> metavar "URI" <> help "Ethereum node endpoint [DEFAULT: Infura mainnet]")
    <*> account
    <*> option str (long "ipfs" <> value "/ip4/127.0.0.1/tcp/5001" <> metavar "MULTIADDR" <> help "IPFS node endpoint [DEFAULT: localhost]")
    <*> option str (long "lighthouse" <> value "airalab.lighthouse.4.robonomics.eth" <> metavar "ENS" <> help "Robonomics lighthouse name")
    <*> option str (long "ens" <> value "0x314159265dD8dbb310642f98f50C066173C1259b" <> metavar "ADDRESS" <> help "ENS registry contract")

account :: Parser LocalKey
account = LocalKey . (importKey :: HexString -> PrivateKey)
    <$> option str (long "private" <> metavar "KEY" <> help "Hex encoded private key")
    <*> option chain (long "chain" <> value foundation <> metavar "CHAIN_ID" <> help "Ethereum chain [foundation, ropsten, kovan, rikenby]")

chain :: ReadM Integer
chain = eitherReader go
  where
    go "foundation" = Right foundation
    go "ropsten"    = Right ropsten
    go "kovan"      = Right kovan
    go "rikenby"    = Right rikenby
    go str | valid str && (read str :: Integer) > 0 = Right (read str)
           | otherwise = Left "CHAIN_ID shoud be [foundation, ropsten, kovan, rikenby] or positive integer"
    valid = all $ flip elem ("1234567890" :: String)

infura :: Provider
infura = HttpProvider "https://mainnet.infura.io/v3/1ba07380f3e740148f89852159695c73"
