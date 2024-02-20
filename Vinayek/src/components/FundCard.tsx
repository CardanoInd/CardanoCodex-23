import { maestroClient } from "@/server/maestro";
import { useWallet } from "@meshsdk/react";
import { useEffect, useState } from "react";
import { Transaction } from "@meshsdk/core";

export const FundCard = ({
  address,
  amount,
  description,
  title,
}: {
  title: string;
  description: string;
  address: string;
  amount: string;
}) => {
  const { wallet } = useWallet();

  const [funded, setFunded] = useState(false);

  const fund = async () => {
    try {
      const tx = new Transaction({ initiator: wallet }).sendLovelace(
        address,
        amount
      );
      const unsignedTx = await tx.build();
      const signedTx = await wallet.signTx(unsignedTx);
      await wallet.submitTx(signedTx);

      setFunded(true);
    } catch (error) {
      console.log(error);
    }
  };

  return (
    <div className="border rounded-lg p-4 flex flex-col items-start gap-y-4">
      <div className="font-medium">{title}</div>
      <div className="font-light text-xs">{description}</div>
      <div className="flex items-end justify-end w-full">
        <button
          onClick={fund}
          className="rounded-full border px-6 py-2 cursor-pointer"
        >
          Fund now
        </button>
      </div>
      {funded && <div className="text-center w-full">Thanks!</div>}
    </div>
  );
};
