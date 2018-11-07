{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Network.Robonomics.Contract.Lighthouse where

import           Network.Ethereum.Contract.TH

[abiFrom|abi/LighthouseLib.json|]
