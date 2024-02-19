{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Rest.Utils where

import           Cardano.Api
import           Cardano.Api.Shelley   (Address (..), PlutusScript (..))
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Codec.Serialise       (serialise)
import           Data.Aeson            (decode, encode)
import           Data.Aeson.Encode.Pretty
                   (Config (..), encodePretty', defConfig, keyOrder)
import           Data.Aeson.Text       (encodeToLazyText)
import           Data.Text                   (pack)
import           Data.Maybe                  (fromMaybe)
import qualified Ledger
import qualified Ledger as Plutus
import           Ledger.Value                as Value
import           Plutus.PAB.Webserver.Types  (ContractActivationArgs (..))
import           PlutusTx              (Data (..))
import           PlutusTx.Trace              (traceError)
import qualified PlutusTx
import           PlutusTx.Builtins           (toBuiltin)
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           Plutus.V1.Ledger.Scripts    (Datum(..))
import qualified Plutus.V2.Ledger.Api as PV2

import           Wallet.Emulator.Wallet      (WalletId (..), Wallet (..))
import           Wallet.Types                (ContractInstanceId (..))
import           System.Process
import           GHC.IO.Handle.Text
import Sample.Contracts.Types
import Text.Printf (printf)
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import Ledger.Constraints hiding (adjustUnbalancedTx)
import Plutus.Contract

adjustAndSubmit :: ( PlutusTx.FromData (Scripts.DatumType a)
                   , PlutusTx.ToData (Scripts.RedeemerType a)
                   , PlutusTx.ToData (Scripts.DatumType a)
                   , AsContractError e
                   )
                => Scripts.TypedValidator a
                -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                -> Contract w s e Ledger.CardanoTx
adjustAndSubmit inst = adjustAndSubmitWith $ typedValidatorLookups inst

adjustAndSubmitWith :: ( PlutusTx.FromData (Scripts.DatumType a)
                       , PlutusTx.ToData (Scripts.RedeemerType a)
                       , PlutusTx.ToData (Scripts.DatumType a)
                       , AsContractError e
                       )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e Ledger.CardanoTx
adjustAndSubmitWith lookups constraints = do
    utx <- (mkTxConstraints lookups constraints) >>= adjustUnbalancedTx
    --logDebug @String $ printf "unbalancedTx: %s" $ show utx
    unsigned <- balanceTx utx
    --logDebug @String $ printf "balanced: %s" $ show unsigned
    signed <- submitBalancedTx unsigned
    --logDebug @String $ printf "signed: %s" $ show signed
    return signed

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

stakeReferenceLedgerToPlutus :: Ledger.StakeReference StandardCrypto -> Maybe Plutus.StakingCredential
stakeReferenceLedgerToPlutus (StakeRefBase x)                   = Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (StakeRefPtr (Ptr (SlotNo x) y z)) = Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus StakeRefNull                       = Nothing

credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (ScriptHash h)) = PV2.ScriptCredential $ PV2.ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = PV2.PubKeyCredential $ PV2.PubKeyHash $ toBuiltin $ hashToBytes h

getCredentials :: Plutus.Address -> Maybe (Plutus.PaymentPubKeyHash, Maybe Plutus.StakePubKeyHash)
getCredentials (Plutus.Address x y) = case x of
    ScriptCredential _   -> Nothing
    PubKeyCredential pkh ->
      let
        ppkh = Plutus.PaymentPubKeyHash pkh
      in
        case y of
            Nothing                        -> Just (ppkh, Nothing)
            Just (Plutus.StakingPtr _ _ _) -> Nothing
            Just (StakingHash h)           -> case h of
                ScriptCredential _    -> Nothing
                PubKeyCredential pkh' -> Just (ppkh, Just $ Plutus.StakePubKeyHash pkh')

unsafePaymentPubKeyHash :: Plutus.Address -> Plutus.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (error $ "script address " ++ show addr ++ " does not contain a payment key") fst $ getCredentials addr

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ pack x of
    Nothing                                      -> Nothing
    Just (AddressByron _)                        -> Nothing
    Just (AddressShelley (ShelleyAddress _ p s)) -> Just Plutus.Address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }

tryReadWalletId :: String -> Maybe WalletId
tryReadWalletId = decode . encode

unsafeReadWalletId :: String -> WalletId
unsafeReadWalletId s = fromMaybe (error $ "can't parse " ++ s ++ " as a WalletId") $ tryReadWalletId s

unsafeReadAddress :: String -> Plutus.Address
unsafeReadAddress s = fromMaybe (error $ "can't parse " ++ s ++ " as an address") $ tryReadAddress s

contractActivationArgs :: WalletId -> a -> ContractActivationArgs a
contractActivationArgs wid a = ContractActivationArgs
    { caID = a
    , caWallet = Just $ Wallet { getWalletId = wid
                               }
    }


{-# INLINEABLE selectDatum #-}
selectDatum :: Plutus.TxInfo -> Plutus.TxOut -> Datum
selectDatum info txOut = case Plutus.txOutDatumHash txOut of
  Nothing -> traceError "No txOutDatumHash"
  Just dhash ->
    case Plutus.findDatum dhash info of
      Nothing -> traceError "Datum is not found from hash"
      Just dat -> dat

addrToPaymentPubKeyHash :: String -> Plutus.PaymentPubKeyHash
addrToPaymentPubKeyHash addr = unsafePaymentPubKeyHash $ unsafeReadAddress addr
