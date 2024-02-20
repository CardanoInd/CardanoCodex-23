import Image from "next/image";
import { Poppins } from "next/font/google";
import { CardanoWallet } from "@meshsdk/react";
import { MeshProvider, useWallet } from "@meshsdk/react";
import { useContext, useEffect, useMemo, useState } from "react";
import { createTransaction, signTransaction } from "@/server";
import { WalletContext } from "@/context/wallet";
import { CLUBS } from "@/context/data";
import { useRouter } from "next/router";

const inter = Poppins({
  weight: ["100", "200", "300", "400", "500", "600", "700", "800", "900"],
  subsets: ["latin"],
});

export default function Home() {
  const { wallet } = useWallet();
  
  const contextData = useContext(WalletContext);

  async function startMinting(club_id: string) {
    try {
      const clubData = CLUBS.find((club) => club.id === club_id);

      if (clubData) {
        const price = clubData ? clubData?.price : "";
        const recipientAddress = await wallet.getChangeAddress();
        const utxos = await wallet.getUtxos();

        const { assetName, maskedTx, originalMetadata } =
          await createTransaction(
            recipientAddress,
            utxos,
            price,
            clubData.type
          );

        const signedTx = await wallet.signTx(maskedTx, true);

        const { appWalletSignedTx } = await signTransaction(
          assetName,
          signedTx,
          originalMetadata
        );

        const txHash = await wallet.submitTx(appWalletSignedTx);

        contextData?.saveClubTokens([
          ...contextData.club_tokens,
          {
            name: clubData.id,
            type: clubData.type,
          },
        ]);
      }
    } catch (error) {
      console.error(error);
    }
  }

  const allowedClubs = useMemo(
    () => contextData?.club_tokens.map((token) => token.type),
    [contextData?.club_tokens]
  );

  useEffect(() => {
    console.log(contextData?.club_tokens);
  }, [contextData]);

  const router = useRouter();

  return (
    <main className={`${inter.className}`}>
      <div className="px-16">
        <div className="text-2xl mt-10">Featured Clubs</div>
        <div className="flex gap-x-4 justify-around mt-10">
          {CLUBS.map((club) => (
            <div key={club.id} className="rounded-xl bg-[#282828] p-4">
              <div className="">
                <Image
                  className="rounded-xl w-[400px] h-[200px] object-cover"
                  src={club.image}
                  width={400}
                  height={1000}
                  alt="club"
                />
              </div>
              <div className="flex justify-between items-end gap-x-4 mt-4 w-full">
                <div className="">
                  <div className="text-lg">{club.title}</div>
                  <div className="font-light mt-4">
                    <div className="">Perks</div>
                    <ul className="text-sm list-disc ml-4">
                      {club.perks.map((perk) => (
                        <li key={perk}>{perk}</li>
                      ))}
                    </ul>
                  </div>
                </div>
                <button
                  onClick={() => {
                    allowedClubs?.includes(club.type)
                      ? router.push(`/${club.id}`)
                      : startMinting(club.id);
                  }}
                  className="rounded-full border px-6 py-2"
                >
                  {allowedClubs?.includes(club.type) ? "Enter" : "Join"}
                </button>
              </div>
            </div>
          ))}
        </div>
      </div>
    </main>
  );
}
