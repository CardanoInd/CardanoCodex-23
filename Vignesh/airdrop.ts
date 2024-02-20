import { Transaction } from "npm:@meshsdk/core";
import chalk from "npm:chalk";
import { csvToJson } from "./helper.ts";
import { blockfrost } from "./config.ts";
import { wallet } from "./config.ts";

const aidropData = csvToJson(await Deno.readTextFile("Sample.csv")) as {
  Address: string;
  Token: string;
}[];

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

const combine_recurring = aidropData.reduce(
  (accumulator, { Address, Token }) => {
    const account = accumulator[Address];
    if (account) {
      accumulator[Address].push(Token);
    } else {
      accumulator[Address] = [Token];
    }
    return accumulator;
  },
  {} as { [key: string]: string[] }
);

const mapped = Object.entries(combine_recurring).map(([Address, tokens]) => ({
  Address,
  Tokens: tokens,
}));

console.log(chalk.blue(`Starting the airdrop now!`));

for (const { Address, Tokens } of mapped) {
  const [_address] = await blockfrost.accountsAddresses(Address);

  const tx = new Transaction({ initiator: wallet }).sendAssets(
    _address.address,
    [
      ...Tokens.map((asset) => ({
        unit: asset,
        quantity: "1",
      })),
    ]
  );

  const unsignedTx = await tx.build();
  const signedTx = await wallet.signTx(unsignedTx);
  const txHash = await wallet.submitTx(signedTx);

  console.log(chalk.cyan(`-------------------------------------------------`));
  console.log(chalk.green(`Airdropped ${Tokens.length} Tokens: ${Address}`));
  console.log(chalk.gray(`Transaction hash: ${txHash.slice(30)}...`));
  console.log(chalk.cyan(`-------------------------------------------------`));

  await sleep(2 * 60 * 1000);
}
