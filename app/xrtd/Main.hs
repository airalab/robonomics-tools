{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Logger                   (LoggingT,
                                                         runStderrLoggingT)
import           Crypto.Random                          (MonadRandom (..))
import           Data.Version                           (showVersion)
import           Options.Applicative

import qualified Network.Robonomics.Lighthouse.Provider as Provider (ipfs,
                                                                     local)
import           Options
import           Paths_robonomics_tools

main :: IO ()
main = run =<< execParser opts
  where
    opts = info
        (options <**> helper)
        (fullDesc <> progDesc ("XRTd v" ++ showVersion version ++ " :: Robonomics network provider"))

run :: Options -> IO ()
run (Options config local)  = runStderrLoggingT $ provider config
  where provider | local = Provider.local
                 | otherwise = Provider.ipfs

instance MonadRandom (LoggingT IO) where
    getRandomBytes = liftIO . getRandomBytes
