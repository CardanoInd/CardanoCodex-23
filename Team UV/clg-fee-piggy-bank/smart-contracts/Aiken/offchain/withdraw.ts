import {
    Blockfrost,
    C,
    Data,
    Lucid,
    SpendingValidator,
    TxHash,
    fromHex,
    toHex,
  } from "https://deno.land/x/lucid@0.8.3/mod.ts";
  import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";
   
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      Deno.env.get(BLOCKFROST_API_KEY),
    ),
    "Preprod",
  );
   
  lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./beneficiary.sk"));
   
  const beneficiaryPublicKeyHash = lucid.utils.getAddressDetails(
    await lucid.wallet.address()
  ).paymentCredential.hash;
   
  const validator = await readValidator();
   
  // --- Supporting functions
   
  async function readValidator(): Promise<SpendingValidator> {
    const validator = JSON.parse(await Deno.readTextFile("plutus.json")).validators[0];
    return {
      type: "PlutusV2",
      script: toHex(cbor.encode(fromHex(validator.compiledCode))),
    };
  }


  // ^^^ Code above is unchanged. ^^^
 
const scriptAddress = lucid.utils.validatorToAddress(validator);
 
// we get all the UTXOs sitting at the script address
const scriptUtxos = await lucid.utxosAt(scriptAddress);
 
const Datum = Data.Object({
  lock_until: Data.BigInt, // this is POSIX time, you can check and set it here: https://www.unixtimestamp.com
  owner: Data.String, // we can pass owner's verification key hash as byte array but also as a string
  beneficiary: Data.String, // we can beneficiary's hash as byte array but also as a string
});
 
type Datum = Data.Static<typeof Datum>;
 
const currentTime = new Date().getTime();
 
// we filter out all the UTXOs by beneficiary and lock_until
const utxos = scriptUtxos.filter((utxo) => {
    let datum = Data.from<Datum>(
      utxo.datum,
      Datum,
    );
 
    return datum.beneficiary === beneficiaryPublicKeyHash &&
      datum.lock_until <= currentTime;
});
 
if (utxos.length === 0) {
  console.log("No redeemable utxo found. You need to wait a little longer...");
  Deno.exit(1);
}
 
// we don't have any redeemer in our contract but it needs to be empty
const redeemer = Data.empty();
 
const txUnlock = await unlock(utxos, currentTime, { from: validator, using: redeemer });
 
await lucid.awaitTx(txUnlock);
 
console.log(`1 tADA recovered from the contract
    Tx ID: ${txUnlock}
    Redeemer: ${redeemer}
`);
 
// --- Supporting functions
 
async function unlock(utxos, currentTime, { from, using }): Promise<TxHash> {
  const laterTime = new Date(currentTime + 2 * 60 * 60 * 1000).getTime(); // add two hours (TTL: time to live)
 
  const tx = await lucid
    .newTx()
    .collectFrom(utxos, using)
    .addSigner(await lucid.wallet.address()) // this should be beneficiary address
    .validFrom(currentTime)
    .validTo(laterTime)
    .attachSpendingValidator(from)
    .complete();
 
  const signedTx = await tx
    .sign()
    .complete();
 
  return signedTx.submit();
}