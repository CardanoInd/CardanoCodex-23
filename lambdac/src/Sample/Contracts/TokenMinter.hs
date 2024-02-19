{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- for Playground imports
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Sample.Contracts.TokenMinter (
  module Sample.Contracts.TokenMinter,
  module Sample.Contracts.Types
  ) where


import Cardano.Api hiding (TxOut, Value, getTxId)
import Control.Lens (view)
import Control.Monad hiding (fmap)
import Data.Maybe (fromJust)
import Data.Aeson
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Map as Map hiding (filter)
import qualified Data.Maybe as DM (fromJust)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints
import Ledger.Constraints as Constraints
import Ledger.Tx.CardanoAPI
import Ledger.Value as Value
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage) -- printJson, printSchemas, stage and ensureKnownCurrencies for the Playground
import qualified Data.OpenApi.Schema as OpenApi (ToSchema)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Plutus.Contract as Contract
import Plutus.Contract.Request as Request
import Plutus.Contract.Wallet (getUnspentOutput)
import Plutus.Rest.Utils (tryReadAddress, selectDatum, adjustAndSubmit, adjustAndSubmitWith)
import Plutus.Script.Utils.V1.Scripts qualified as Scripts
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Value (flattenValue)
import PlutusTx.Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless, head)
import Prelude (IO, Semigroup (..), Show (..), String, div, head, last, fromIntegral)
import qualified Data.Map as Map hiding (filter)
import qualified Data.Maybe as DM (fromJust)
import qualified Data.OpenApi.Schema as OpenApi (ToSchema)
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import qualified PlutusTx
import Plutus.V1.Ledger.Value (flattenValue)
import qualified Prelude
import PlutusTx.Prelude hiding (Semigroup (..), unless, head)
import PlutusTx.Builtins
-- IO for Playground
import Text.Printf (printf)
import Prelude (IO, Semigroup (..), Show (..), String, div, head, last)
import Sample.Contracts.Types
import Plutus.Rest.Utils (adjustAndSubmit, adjustAndSubmitWith)

PlutusTx.makeLift ''MintingPolicyParam
PlutusTx.makeIsDataIndexed ''MintingPolicyParam [('MintingPolicyParam, 0)]


{-# INLINABLE mkMintPolicy #-}
mkMintPolicy :: MintingPolicyParam -> () -> ScriptContext -> Bool
mkMintPolicy mp () ctx = traceIfFalse "Not signed" validSignature
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validSignature :: Bool
    validSignature = txSignedBy info (unPaymentPubKeyHash $ issuer mp)

mintPolicy :: MintingPolicyParam -> Scripts.MintingPolicy
mintPolicy mp = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mp'->  Scripts.mkUntypedMintingPolicy $ mkMintPolicy mp' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp

{-# INLINABLE curSymbol #-}
curSymbol :: MintingPolicyParam -> CurrencySymbol
curSymbol = Scripts.scriptCurrencySymbol . mintPolicy

type MintSchema =
  Endpoint "mintToken" MintParams

toMintingPolicyParams :: PaymentPubKeyHash -> String -> MintingPolicyParam
toMintingPolicyParams p tn =
  MintingPolicyParam
    { issuer = p
    , tName = TokenName { unTokenName = stringToBuiltinByteString tn}
    }

mintToken :: MintParams -> Contract w s Text ()
mintToken rp = do
  oref <- getUnspentOutput
  o <- fromJust <$> Contract.txOutFromRef oref
  ownPkh <- Request.ownFirstPaymentPubKeyHash
  let mp = toMintingPolicyParams ownPkh (tn rp)
      val = Value.singleton (curSymbol mp) (tName mp) (amt rp)
      lookups     = Constraints.mintingPolicy (mintPolicy mp) <>
                    Constraints.unspentOutputs (Map.singleton oref o)
      constraints = mustMintValue val
                  <> mustSpendPubKeyOutput oref
                  <> mustPayToPubKey (clientPkh rp) val
                  <> mustBeSignedBy ownPkh
  void $ adjustAndSubmitWith @Scripts.Any lookups constraints
  logInfo @String $ printf "minted %s" (show val)

mintToken' :: Promise () MintSchema Text ()
mintToken' = endpoint @"mintToken" mintToken

endpoints ::  Contract () MintSchema Text ()
endpoints = do
  logInfo @String "Waiting for some action."
  selectList [mintToken'] >> endpoints

mkSchemaDefinitions ''MintSchema
mkKnownCurrencies []
