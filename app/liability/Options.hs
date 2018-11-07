{-# LANGUAGE OverloadedStrings #-}
module Options where

import           Data.Solidity.Prim.Address (Address, fromHexString)
import           Network.Ethereum.Api.Types (DefaultBlock (..), Quantity)
import           Options.Applicative

newtype Options = Options {
  optionsCommand :: Command
}

data Command
    = Read Address
    | List Address DefaultBlock DefaultBlock

options :: Parser Options
options = Options <$> commandOptions

commandOptions :: Parser Command
commandOptions = subparser $
    command "read" (info readOptions (progDesc "Read liability smart contract"))
 <> command "list" (info listOptions (progDesc "List liability smart contracts"))

readOptions :: Parser Command
readOptions = Read <$> argument str (metavar "ADDRESS" <> help "Liablity contract address")

block :: ReadM DefaultBlock
block = eitherReader go
  where
    valid = all $ flip elem ("1234567890-" :: String)
    go "-" = Right Latest
    go str | valid str && (read str :: Integer) >= 0 = Right $ BlockWithNumber (fromInteger $ read str)
           | otherwise     = Left "Block number should be positive integer or '-'"

listOptions :: Parser Command
listOptions = List
    <$> argument str (metavar "ADDRESS" <> help "Liability factory contract address" <> value "0xBd127854f5F9022B72459ca97EcE166df9B3012D")
    <*> option block (long "from" <> value (BlockWithNumber 0) <> help "Start block number")
    <*> option block (long "to" <> value Latest <> help "End block number")
