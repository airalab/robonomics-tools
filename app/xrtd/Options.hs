{-# LANGUAGE OverloadedStrings #-}
module Options where

import           Crypto.Ethereum                        (PrivateKey)
import           Crypto.Ethereum.Utils                  (importKey)
import           Data.ByteArray.HexString               (HexString)
import           Network.Ethereum.Account.LocalKey      (LocalKey (..))
import           Network.Ethereum.Api.Provider          (Provider (..))
import           Network.Ethereum.Chain
import           Network.Ethereum.Unit                  (Shannon, fromWei)
import           Options.Applicative

import qualified Network.Robonomics.Lighthouse.Provider as P

infura :: Provider
infura = HttpProvider "https://mainnet.infura.io/v3/1ba07380f3e740148f89852159695c73"

data Options = Options
    { providerConfig :: !P.Config
    , providerLocal  :: !Bool
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> providerOptions
    <*> switch (long "local" <> short 'l' <> help "Run in local mode: don't listen messages but generate it on local machine")

providerOptions :: Parser P.Config
providerOptions = P.Config
    <$> option (HttpProvider <$> str) (long "web3" <> value infura <> metavar "URI" <> help "Ethereum node endpoint [DEFAULT: Infura mainnet]")
    <*> account
    <*> option str (long "ipfs" <> value "/ip4/127.0.0.1/tcp/5001" <> metavar "MULTIADDR" <> help "IPFS node endpoint [DEFAULT: localhost]")
    <*> option str (long "lighthouse" <> value "airalab.lighthouse.5.robonomics.eth" <> metavar "ENS" <> help "Robonomics lighthouse name")
    <*> option str (long "factory" <> value "factory.5.robonomics.eth" <> metavar "ENS" <> help "Robonomics liability factory name")
    <*> option str (long "ens" <> value "0x314159265dD8dbb310642f98f50C066173C1259b" <> metavar "ADDRESS" <> help "ENS registry contract")
    <*> option gasprice (long "gasprice" <> value P.SafePrice <> metavar "GAS_PRICE" <> help "Transaction gas price (safe, fast, fastest or in GWei) [DEFAULT: safe]")

account :: Parser LocalKey
account = LocalKey . (importKey :: HexString -> PrivateKey)
    <$> option str (long "private" <> metavar "KEY" <> help "Hex encoded private key")
    <*> option chain (long "chain" <> value foundation <> metavar "CHAIN_ID" <> help "Ethereum chain [foundation, ropsten, kovan, rikenby]")

gasprice :: ReadM P.GasPrice
gasprice = eitherReader go
  where
    go "safe" = Right P.SafePrice
    go "fast" = Right P.FastPrice
    go "fastest" = Right P.FastestPrice
    go str | valid str && (read str :: Integer) > 0 = Right . P.GasPrice . shannon $ read str
           | otherwise = Left "GAS_PRICE shoud be [safe, fast, fastest] or in GWei"
    valid = all $ flip elem ("1234567890" :: String)
    shannon :: Integer -> Shannon
    shannon  = fromWei . (* 10^9)


chain :: ReadM Integer
chain = eitherReader go
  where
    go "foundation" = Right foundation
    go "ropsten"    = Right ropsten
    go "kovan"      = Right kovan
    go "rikenby"    = Right rikenby
    go "sidechain"  = Right 0x1163      -- Airalab sidechain
    go str | valid str && (read str :: Integer) > 0 = Right (read str)
           | otherwise = Left "CHAIN_ID shoud be [foundation, ropsten, kovan, rikenby] or positive integer"
    valid = all $ flip elem ("1234567890" :: String)
