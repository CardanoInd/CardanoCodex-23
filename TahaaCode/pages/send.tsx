import Head from "next/head";
import { CardanoWallet, useWallet } from "@meshsdk/react";
import { Transaction } from "@meshsdk/core";
import { useSearchParams } from "next/navigation";
import { useState } from "react";
import { toast } from "react-toastify";

export default function Home() {
  const searchParams = useSearchParams();
  const [sent, setSent] = useState<string | null>(null);

  const { wallet, connected } = useWallet();

  const sendAmount = async () => {
    try {
      const amount = searchParams.get("amount");
      const address = searchParams.get("address");

      if (!connected) {
        toast.error("Connect wallet");
        return;
      }

      if (!amount) {
        toast.error("Please put amount!");
        return;
      }

      if (!address) {
        toast.error("Please put address!");
        return;
      }

      if (amount && address && connected) {
        const tx = new Transaction({ initiator: wallet }).sendLovelace(
          address,
          amount
        );
        const unsignedTx = await tx.build();
        const signedTx = await wallet.signTx(unsignedTx);
        const txHash = await wallet.submitTx(signedTx);

        setSent(txHash);

        toast.success("Sent!");
      }
    } catch (error) {
      console.log(error);
    }
  };
  return (
    <div className="container">
      <Head>
        <title>Pull Pay</title>
      </Head>

      <main>
        <div className="min-h-screen bg-gray-100 flex items-center justify-center">
          <div className="bg-white p-8 rounded-lg shadow-lg w-full max-w-md">
            <div className="text-2xl font-bold text-center py-2 pb-10">
              Pull Pay
            </div>

            <div className="w-full flex items-center">
              <CardanoWallet />
            </div>
            <div className="space-y-4">
              <div className="py-4">
                Sending to: {searchParams.get("address")?.slice(0, 16)}...
                {searchParams
                  .get("address")
                  ?.slice((searchParams.get("address")?.length ?? 0) - 20)}
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700">
                  Amount:
                </label>
                {parseInt(searchParams.get("amount") ?? "0") / 1000000}
              </div>
              <div className="flex flex-col justify-end">
                <button
                  onClick={sendAmount}
                  className="px-6 py-2 border border-transparent text-sm font-medium rounded-md text-white bg-indigo-600 shadow-sm hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                >
                  {sent ? "Sent" : "Send"}
                </button>
                {sent && (
                  <div className="text-center py-4">
                    Transaction Link:
                    {sent && (
                      <a href={`https://cardanoscan.io/transaction/${sent}`}>
                        {sent}
                      </a>
                    )}
                  </div>
                )}
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}
