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

module Sample.Contracts.Escrow (
  module Sample.Contracts.Escrow,
  module Sample.Contracts.Types
  ) where

import Cardano.Api hiding (TxOut, Value, getTxId)
import Control.Lens (view)
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Either.Unwrap
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints hiding (adjustUnbalancedTx)
import Ledger.Tx.CardanoAPI
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage) -- printJson, printSchemas, stage and ensureKnownCurrencies for the Playground
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import Plutus.Contract (adjustUnbalancedTx)
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
import qualified Prelude
import Sample.Contracts.Types
import Text.Printf (printf)

data EscrowDatum = Party PaymentPubKeyHash
                      deriving (Show)

data EscrowRedeemer = Lock | Cancel | Collect
                      deriving (Show)

adaInLoveLace :: Integer -> Integer
adaInLoveLace = (* 1000000)

lovelaceToAda :: Integer -> Integer
lovelaceToAda i = (fromIntegral i) `div` 1000000

PlutusTx.makeLift ''EscrowParams
PlutusTx.makeIsDataIndexed ''EscrowDatum [('Party, 0)]
PlutusTx.makeIsDataIndexed ''EscrowRedeemer [('Lock, 0),('Cancel, 1),('Collect, 2)]

{-# INLINEABLE mkValidator #-}
mkValidator :: EscrowParams -> EscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
mkValidator EscrowParams{buyer, seller, lovelaceAmt,finaliseTime,endTime} d r ctx =
  (traceIfFalse "Missing valid signature" $ validSignature buyer seller r) &&
  (traceIfFalse "Invalid inputs" $ validInputs buyer d r)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validSignature :: PaymentPubKeyHash -> PaymentPubKeyHash -> EscrowRedeemer -> Bool
    validSignature buyer seller r =
      case r of
        Cancel -> txSignedBy info (unPaymentPubKeyHash buyer)
        Collect -> txSignedBy info (unPaymentPubKeyHash seller)
        _ -> traceError "Invalid redeemer"

    validInputs buyer (Party pkh) r =
      case r of
        Cancel -> (pkh == buyer) && (allSatisfyOwnUtxos (Party buyer))
        _ -> True

    allSatisfyOwnUtxos :: EscrowDatum -> Bool
    allSatisfyOwnUtxos dat =
      and $
        map (satisfyDatum dat) $
          filter (isJust . toValidatorHash . txOutAddress) (txInInfoResolved <$> txInfoInputs info)

    satisfyDatum :: EscrowDatum -> TxOut -> Bool
    satisfyDatum dat txOut = case txOutDatumHash txOut of
      Nothing -> False
      _ -> case selectDatum info txOut of
        dat -> True
        _ -> False

{-# INLINEABLE parseDatumValue #-}
parseDatumValue :: TxInfo -> TxOut -> EscrowDatum
parseDatumValue info txOut = do
  let (Datum d) = selectDatum info txOut in
   case PlutusTx.fromBuiltinData d of
    Nothing -> traceError "Datum value is empty!"
    Just p -> p


data Escrow
instance Scripts.ValidatorTypes Escrow where
  type instance RedeemerType Escrow = EscrowRedeemer
  type instance DatumType Escrow = EscrowDatum

typedValidator :: EscrowParams -> Scripts.TypedValidator Escrow
typedValidator = Scripts.mkTypedValidatorParam @Escrow
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator

validator :: EscrowParams -> Scripts.Validator
validator = Scripts.validatorScript . typedValidator

scrAddress :: EscrowParams -> Ledger.Address
scrAddress = scriptAddress . validator

contractDatum :: PaymentPubKeyHash -> Datum
contractDatum pkh =  Datum (PlutusTx.toBuiltinData $ Party pkh)

type EscrowSchema =
  Endpoint "lock" EscrowParams
    .\/ Endpoint "cancel" EscrowParams
    .\/ Endpoint "collect" EscrowParams

lock :: EscrowParams -> Contract w s Text ()
lock ep = do
  pkh <- Request.ownFirstPaymentPubKeyHash
  logInfo @String $ "Wallet pkh : " <> show pkh
  let dat = Party pkh
      tx = mustPayToTheScript dat $ Ada.lovelaceValueOf (lovelaceAmt ep)
  txM <- adjustAndSubmitWith @Escrow (typedValidatorLookups (typedValidator ep)) tx
  logInfo @String $
    printf
      "Locked a value of of %d ADA to escrow contract with datum %s"
      (lovelaceToAda $ lovelaceAmt ep)
      (show dat)

collect :: EscrowParams -> Contract w s Text ()
collect ep = do
  pkh <- Request.ownFirstPaymentPubKeyHash
  logInfo @String $ "Wallet pkh : " <> show pkh
  logInfo @String $ "Collecting funds from contract : " <> show ep
  utxos <- utxosAt $ scrAddress ep
  --logInfo @String $ printf "%s" (show utxos)
  logInfo @String $ "Script address is" <> show (serialiseAddress $ fromRight (toCardanoAddressInEra (Mainnet) $ scrAddress ep))
  os <- utxosAt (scrAddress ep)
  let totalVal = mconcat [view ciTxOutValue o | o <- map snd (Map.toList os)]
      datums = [datumContent o | o <- map snd (Map.toList os)]
  logInfo @String $ "Total Value at script: " <> show totalVal
  logInfo @String $ "Datums at script: " <> show datums
  if Map.null utxos
    then logInfo @String $ "No funds at the contract."
    else do
    let tx = collectFromScript utxos Collect
             <> mustBeSignedBy pkh
    txM <- submitTxConstraintsSpending (typedValidator ep) utxos tx
    logInfo @String $ "Executed Collect."

cancel :: EscrowParams -> Contract w s Text ()
cancel ep = do
  pkh <- Request.ownFirstPaymentPubKeyHash
  logInfo @String $ "Wallet pkh : " <> show pkh
  let dat = Party pkh
  os <- utxosAt (scrAddress ep)
  let totalVal = mconcat [view ciTxOutValue o | o <- map snd (Map.toList os)]
      datums = [datumContent o | o <- map snd (Map.toList os)]
  logInfo @String $ "Total Value at script: " <> show totalVal
  logInfo @String $ "Datums at script: " <> show datums
  let ownUtxoFilter _ ciTxOut = either id Scripts.datumHash (_ciTxOutDatum ciTxOut) == Scripts.datumHash (Datum (PlutusTx.toBuiltinData dat))
      tx = collectFromScriptFilter ownUtxoFilter os Cancel
            <> mustIncludeDatum (Datum $ PlutusTx.toBuiltinData dat)
            <> mustBeSignedBy (buyer ep)
      lps = typedValidatorLookups (typedValidator ep)
            <> Ledger.Constraints.unspentOutputs (ownUtxo ownUtxoFilter os)
  txM <- adjustAndSubmitWith @Escrow lps tx
  logInfo @String $ "Escrow cancelled."

ownUtxo :: (TxOutRef -> ChainIndexTxOut -> Bool) -> Map.Map TxOutRef ChainIndexTxOut -> Map.Map TxOutRef ChainIndexTxOut
ownUtxo flt uxos = Map.filterWithKey flt uxos

datumContent :: ChainIndexTxOut -> Maybe EscrowDatum
datumContent o = do
  Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
  PlutusTx.fromBuiltinData d

lock' :: Promise () EscrowSchema Text ()
lock' = endpoint @"lock" lock

cancel' :: Promise () EscrowSchema Text ()
cancel' = endpoint @"cancel" cancel

collect' :: Promise () EscrowSchema Text ()
collect' = endpoint @"collect" collect

endpoints ::  Contract () EscrowSchema Text ()
endpoints = do
  logInfo @String "Waiting for some action."
  selectList [lock', cancel', collect'] >> endpoints

mkSchemaDefinitions ''EscrowSchema
mkKnownCurrencies []
