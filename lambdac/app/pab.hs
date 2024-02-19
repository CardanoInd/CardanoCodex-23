{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
    ( main
    ) where

import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                       (runWith)

import           ProjectPAB                         (PABContracts(..))
import           Prelude (IO, Semigroup (..), Show (..), String, div)

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @PABContracts)
