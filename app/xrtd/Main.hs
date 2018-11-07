{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Logger                   (runStderrLoggingT)
import           Options.Applicative

import qualified Network.Robonomics.Lighthouse.Provider as Provider (ipfs)
import           Options

main :: IO ()
main = run =<< execParser opts
  where
    opts = info
        (options <**> helper)
        (fullDesc <> progDesc "XRTd :: Robonomics network provider")

run :: Options -> IO ()
run (Options config) = runStderrLoggingT $ Provider.ipfs config
