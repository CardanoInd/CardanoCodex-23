import { BlockFrostAPI } from "npm:@blockfrost/blockfrost-js";
import { AppWallet, BlockfrostProvider } from "npm:@meshsdk/core";

const BLOCKFROST_KEY = "preprodCgFVVesU4gm4Fy945DaAN5wXFE0fsOJ2";

export const blockfrost = new BlockFrostAPI({
  projectId: BLOCKFROST_KEY,
});

const blockchainProvider = new BlockfrostProvider(BLOCKFROST_KEY);

export const wallet = new AppWallet({
  networkId: 0,
  fetcher: blockchainProvider,
  submitter: blockchainProvider,
  key: {
    type: "mnemonic",
    words: [
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
      "solution",
    ],
  },
});