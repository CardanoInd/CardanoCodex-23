{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProjectPAB
  ( PABContracts(..)
  ) where

import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..))
import           Data.Default                        (def)
import           Data.Text (Text)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import qualified Ledger
import           Data.OpenApi.Schema                 (ToSchema)
import           Plutus.Contract                     (ContractError)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, Builtin,HasDefinitions(..), SomeBuiltin (..), BuiltinHandler(contractHandler), endpointsToSchemas)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Sample.Contracts.Escrow             as Escrow
import qualified Sample.Contracts.TokenMinter        as TokenMinter
import           Sample.Contracts.Types
import           Plutus.Trace.Emulator.Extract       (writeScriptsTo, ScriptsConfig (..), Command (..))
import           Ledger.Index                        (ValidatorMode(..))
import           PlutusTx.Prelude                    hiding (Semigroup (..), unless)
import           Prelude (IO, Semigroup (..), Show (..), String, div)
import qualified Prelude

data PABContracts = Lock EscrowParams
                        | Cancel EscrowParams
                        | Collect EscrowParams
                        | Mint MintParams
    deriving (Eq, Ord, Prelude.Eq, Prelude.Ord, Show, Generic, ToSchema, FromJSON, ToJSON)

instance Pretty PABContracts where
    pretty = viaShow

instance HasDefinitions PABContracts where
    getDefinitions = [ Lock exampleEscrowParams, Cancel exampleEscrowParams
                     , Collect exampleEscrowParams, Mint exampleMintRequestParams
                     ]
    getSchema = const $ endpointsToSchemas @Empty
    getContract = \case
        Lock ep -> SomeBuiltin $ Escrow.lock @() @Empty ep
        Cancel ep -> SomeBuiltin $ Escrow.cancel @() @Empty ep
        Collect ep -> SomeBuiltin $ Escrow.collect @() @Empty ep
        Mint mp -> SomeBuiltin $ TokenMinter.mintToken @() @Empty mp


exampleEscrowParams :: EscrowParams
exampleEscrowParams = EscrowParams
                { buyer = Ledger.PaymentPubKeyHash "ce1e6f3efb2691496a8fa5d31ecc54a47dc887648bf08e157cd887ac"
                , seller = Ledger.PaymentPubKeyHash "356adda224e5a233cfe718838cdb97cc462fdbdd6dd6e35bea8a14ac"
                , lovelaceAmt = 100000000
                , finaliseTime = Ledger.POSIXTime 9876543142987
                , endTime = Ledger.POSIXTime 9876543142987
                }

exampleMintRequestParams :: MintParams
exampleMintRequestParams = MintParams
                { tn = "sampleTokenName"
                , amt = 200000
                , clientPkh = Ledger.PaymentPubKeyHash "ce1e6f3efb2691496a8fa5d31ecc54a47dc887648bf08e157cd887ac"
                }