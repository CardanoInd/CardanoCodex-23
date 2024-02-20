import Head from "next/head";
import { CardanoWallet, MeshBadge, useWallet } from "@meshsdk/react";
import { Transaction } from "@meshsdk/core";
import { useState } from "react";

export default function Home() {
  const [sendAmount, setSendAmount] = useState(1);
  const { wallet } = useWallet();
  async function sendToPiggyBank() {
    try {
      const tx = new Transaction({ initiator: wallet }).sendLovelace(
        "addr_test1xquew2x79qhuru9zqe56hvl9mlp3wqe5snhgwx9lfj7v8upeju5du2p0c8c2ypnf4we7th7rzupnfp8wsuvt7n9uc0cq8fy3dv",
        (sendAmount * 1000000).toString()
      );
      const unsignedTx = await tx.build();
      const signedTx = await wallet.signTx(unsignedTx);
      const txHash = await wallet.submitTx(signedTx);

      console.log(txHash);
    } catch (error) {
      console.log(error);
    }
  }
  return (
    <div className="container">
      <Head>
        <title>Mesh App on Cardano</title>
        <meta name="description" content="A Cardano dApp powered my Mesh" />
        <link rel="icon" href="https://meshjs.dev/favicon/favicon-32x32.png" />
        <link
          href="https://meshjs.dev/css/template.css"
          rel="stylesheet"
          key="mesh-demo"
        />
      </Head>

      <main className="main">
        <h1 className="title">College Piggy Bank</h1>

        <div className="input_container">
          <input
            type="number"
            onChange={(e) => setSendAmount(parseInt(e.target.value))}
            width={200}
            height={100}
            placeholder=""
            className="amount_input"
          />
          <div className="demo">
            <CardanoWallet />
          </div>
          <button className="send_btn" onClick={sendToPiggyBank}>
            Send
          </button>
        </div>
      </main>

      <footer className="footer">
        <MeshBadge dark={true} />
      </footer>
    </div>
  );
}
