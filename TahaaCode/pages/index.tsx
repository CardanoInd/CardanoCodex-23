import Head from "next/head";
import { useRouter } from "next/dist/client/router";
import { useSearchParams } from "next/navigation";
import { useEffect, useState } from "react";
import { toast } from "react-toastify";
import { CardanoWallet, useWallet } from "@meshsdk/react";

export default function Home() {
  const [amount, setAmount] = useState(1);
  const [address, setAddress] = useState<string>("");

  const [copied, setCopied] = useState(false);

  const { wallet, connected } = useWallet();

  useEffect(() => {
    const addyload = async () => {
      if (connected) {
        const address = await wallet.getChangeAddress();

        setAddress(address);
      }
    };

    addyload();
  }, [connected]);

  return (
    <div className="container">
      <Head>
        <title>Pull Pay</title>
      </Head>
      <div className="">
        <div className="form-container">
          <div className="text-2xl font-bold text-center py-2 pb-10">
            Pull Pay
          </div>
          <div className="">
            <div className="form-field">
              <label className="">
                Put your address here or Connect wallet
              </label>
              <div className="py-4">
                <CardanoWallet />
              </div>
              <input
                onChange={(e) => setAddress(e.target.value)}
                type="text"
                className=""
                placeholder="Enter address"
                value={address}
              />
            </div>
            <div className="form-field">
              <label className="">Amount:</label>
              <input
                onChange={(e) => setAmount(parseInt(e.target.value))}
                type="number"
                className=""
                placeholder="Enter amount"
                value={amount}
              />
            </div>
          </div>
          <button
            onClick={() => {
              if (address.length === 0) {
                toast.error("Address required!");
                return;
              }
              navigator.clipboard.writeText(
                `http://localhost:3000/send?address=${address}&amount=${
                  amount * 1000000
                }`
              );
              setCopied(true);
              toast.success("Copied!");
            }}
            className="form-field"
            type="submit"
          >
            {copied ? "Copied" : "Copy Payment Link"}
          </button>
        </div>
      </div>
    </div>
  );
}
