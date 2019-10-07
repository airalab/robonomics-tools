{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                              (void)
import           Control.Monad.IO.Class                     (liftIO)
import           Control.Monad.Logger                       (LoggingT,
                                                             runStderrLoggingT)
import           Control.Monad.State.Strict                 (runStateT)
import           Crypto.Random                              (MonadRandom (..))
import           Data.Version                               (showVersion)
import           Options.Applicative

import           Network.Robonomics.Lighthouse.MatcherStore (MatcherStore (..))
import qualified Network.Robonomics.Lighthouse.Provider     as Provider (ipfs,
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
run (Options config local)
  | local = runStderrLoggingT $ Provider.local config
  | otherwise = void . runStderrLoggingT $
        runStateT (Provider.ipfs config) emptyStore
  where emptyStore = MatcherStore mempty mempty mempty mempty

instance MonadRandom (LoggingT IO) where
    getRandomBytes = liftIO . getRandomBytes
