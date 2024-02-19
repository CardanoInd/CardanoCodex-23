{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Sample.Runtime where

import           Cardano.Api
import           Cardano.Api.Shelley   (Address (..), PlutusScript (..))
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Codec.Serialise       (serialise)
import qualified Data.Aeson            as Aeson
import           Data.Aeson            (decode, encode)
import           Data.Aeson.Encode.Pretty
                   (Config (..), encodePretty', defConfig, keyOrder)
import           Data.Aeson.Text       (encodeToLazyText)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Lazy.IO   as LT
import           Data.Text                   (pack, Text)
import           Data.Maybe                  (fromJust, fromMaybe)
import qualified Ledger
import qualified Ledger as Plutus
import           Ledger.Value                as Value
import           Plutus.PAB.Webserver.Types (ContractActivationArgs (..))
import           Plutus.Script.Utils.V1.Scripts    (datumHash)
import           PlutusTx              (Data (..))
import qualified PlutusTx
import           PlutusTx.Builtins           (toBuiltin)
import           PlutusTx.Builtins.Class (stringToBuiltinByteString)
import           Plutus.V1.Ledger.Api as PV1
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           Plutus.V1.Ledger.Scripts as PV1
import           Wallet.Emulator.Wallet      (WalletId (..), Wallet (..))
import           Wallet.Types                (ContractInstanceId (..))
import           Sample.Contracts.Escrow as E
import           Plutus.Rest.Utils
import Data.Aeson
import qualified Data.Aeson.Text as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writePlainTextJSON :: ToJSON a => FilePath -> a -> IO ()
writePlainTextJSON file = LT.writeFile file . encodeToLazyText

writeValidator :: FilePath -> PV1.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . PV1.unValidatorScript

writeDatum :: Datum -> IO ()
writeDatum d = writeJSON "/tmp/datum.json" d

writeDatumHash :: PV1.DatumHash -> IO ()
writeDatumHash dh = writePlainTextJSON "/tmp/datumHash.json" dh

{-Customised functions below - Depends on the Plutus contracts you use. Escrow and NFT are used as examples here-}
writeEscrowValidator :: E.EscrowParams -> FilePath -> IO (Either (FileError ()) ())
writeEscrowValidator mp file =
  writeValidator file $ E.validator mp

writeEscrowValidator' :: String -> String -> Integer -> Integer -> Integer -> FilePath -> IO (Either (FileError ()) ())
writeEscrowValidator' b s l f e file =
  let m = E.EscrowParams
          { buyer = unsafePaymentPubKeyHash $ unsafeReadAddress b
          , seller = unsafePaymentPubKeyHash $ unsafeReadAddress s
          , lovelaceAmt = l
          , finaliseTime = Plutus.POSIXTime f
          , endTime = Plutus.POSIXTime e
          }
  in writeEscrowValidator m file

writeJSONEscrowParams :: IO ()
writeJSONEscrowParams = writePlainTextJSON "/tmp/escrowparam.json" $
                      E.EscrowParams
                        { buyer = Plutus.PaymentPubKeyHash "ce1e6f3efb2691496a8fa5d31ecc54a47dc887648bf08e157cd887ac"
                        , seller = Plutus.PaymentPubKeyHash "356adda224e5a233cfe718838cdb97cc462fdbdd6dd6e35bea8a14ac"
                        , lovelaceAmt = 200000000
                        , finaliseTime = Plutus.POSIXTime 1650364419000
                        , endTime = Plutus.POSIXTime 1650864419000
                        }

writeEscrowDatum :: String -> IO ()
writeEscrowDatum addr = writeDatum (escrowDatum addr)

writeEscrowDatumHash :: String -> IO ()
writeEscrowDatumHash addr = writeDatumHash (escrowDatumHash addr)

escrowDatumHash :: String -> PV1.DatumHash
escrowDatumHash addr = datumHash (E.contractDatum
                                      $ unsafePaymentPubKeyHash $ unsafeReadAddress addr)

escrowDatum :: String -> PV1.Datum
escrowDatum addr = E.contractDatum
                    $ unsafePaymentPubKeyHash $ unsafeReadAddress addr

escrowDatum' :: String -> LBS.ByteString
escrowDatum' addr = encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData
                    $ E.contractDatum
                    $ unsafePaymentPubKeyHash $ unsafeReadAddress addr

escrowDatum'' :: String -> Aeson.Value
escrowDatum'' addr = scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData
                    $ E.contractDatum
                    $ unsafePaymentPubKeyHash $ unsafeReadAddress addr
