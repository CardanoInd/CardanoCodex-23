{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Sample.Contracts.TokenMinterTrace where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras                   as Extras
import           Ledger
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, String, show, div)
import           Wallet.Emulator.Wallet
import           Test.Tasty
import qualified Test.Tasty.HUnit                             as HUnit
import           Data.Monoid                                  (Last (..))
import           Plutus.Contract.Trace      as X
import           Plutus.V1.Ledger.Time      (POSIXTime (..))
import           Sample.Contracts.TokenMinter
import           Sample.Contracts.Types

testMintToken :: IO ()
testMintToken = runEmulatorTraceIO mintTokenTrace

issuerW, clientW1 , clientW2 :: Wallet
issuerW = X.knownWallet 1
clientW1  = X.knownWallet 2
clientW2  = X.knownWallet 3

mintRequestParams :: String -> Integer -> PaymentPubKeyHash -> MintParams
mintRequestParams name mintAmt clientPkh = MintParams
  { tn = name
  , amt = mintAmt
  , clientPkh = clientPkh
  }

mintTokenTrace  :: EmulatorTrace ()
mintTokenTrace = do
    h1 <- activateContractWallet issuerW endpoints

    let amt = 100000000
        name = "traceTestToken"
        mintParams = mintRequestParams name amt (mockWalletPaymentPubKeyHash clientW1)
    callEndpoint @"mintToken" h1 mintParams
    void $ Emulator.waitNSlots 6
    callEndpoint @"mintToken" h1 (mintParams {tn = "anotherTraceTestToken", clientPkh = mockWalletPaymentPubKeyHash clientW2})
    void $ Emulator.waitNSlots 1


