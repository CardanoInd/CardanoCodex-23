import type { NextApiRequest, NextApiResponse } from "next";
import {
  AppWallet,
  ForgeScript,
  Transaction,
  KoiosProvider,
  largestFirst,
} from "@meshsdk/core";
import type { Mint } from "@meshsdk/core";
import { wallet } from "../../backend/wallet";
import { bankWalletAddress } from "../../backend/mint";

const ipfs_imgs: { [key: string]: string } = {
  Literature: "QmNj8YA4XHHyS1N7e5h445HLfjJGD814DmQG6gBUCetKXT",
  Blockchain: "QmWH1t6TnJoYtqbpKQTQprjGxm3KrBUYPer2eELCCnJcxk",
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse
) {
  const recipientAddress = req.body.recipientAddress;
  const utxos = req.body.utxos;
  const price = req.body.price;
  const type = req.body.type;

  const blockchainProvider = new KoiosProvider("preprod");

  const appWallet = new AppWallet({
    networkId: 0,
    fetcher: blockchainProvider,
    submitter: blockchainProvider,
    key: {
      type: "mnemonic",
      words: wallet.split(" "),
    },
  });

  const appWalletAddress = appWallet.getPaymentAddress();
  const forgingScript = ForgeScript.withOneSignature(appWalletAddress);

  let selectedAssetId = Math.floor(Math.random() * 10000).toString();

  const assetIdPrefix = "Club";
  const assetMetadata = {
    name: `Club Token ${selectedAssetId.toString().padStart(3, "0")}`,
    image: `ipfs://${ipfs_imgs[type]}`,
    mediaType: "image/jpg",
    type: type,
    description: "This token is issued by CardaClub",
  };

  const assetName = `${assetIdPrefix}${selectedAssetId
    .toString()
    .padStart(3, "0")}`;

  const asset: Mint = {
    assetName: assetName,
    assetQuantity: "1",
    metadata: assetMetadata,
    label: "721",
    recipient: {
      address: recipientAddress,
    },
  };

  const selectedUtxos = largestFirst(price, utxos, true);

  const tx = new Transaction({ initiator: appWallet });
  tx.setTxInputs(selectedUtxos);
  tx.mintAsset(forgingScript, asset);
  tx.sendLovelace(bankWalletAddress, price);
  tx.setChangeAddress(recipientAddress);

  const unsignedTx = await tx.build();

  const originalMetadata = Transaction.readMetadata(unsignedTx);

  const maskedTx = Transaction.maskMetadata(unsignedTx);
  res.status(200).json({ assetName, maskedTx, originalMetadata });
}
