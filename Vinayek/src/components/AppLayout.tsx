import { WalletContext } from "@/context/wallet";
import { maestroClient } from "@/server/maestro";
import { CardanoWallet, useWallet } from "@meshsdk/react";
import { ReactNode, useEffect, useState } from "react";

export const Layout = ({ children }: { children: ReactNode }) => {
  const { wallet, connected } = useWallet();

  const [clubTokens, setClubTokens] = useState<
    {
      name: string;
      type: string;
    }[]
  >([]);

  useEffect(() => {
    const fetchClubs = async () => {
      if (connected) {
        const assets = await wallet.getAssets();

        for (const asset of assets.filter(
          (asset) =>
            asset.policyId ===
            "223ad5f5be7ef559d783e48cd408106cc651930ebc2546c5de05ac27"
        )) {
          const { data } = await maestroClient.assets.assetInfo(asset.unit);

          const metadata = data.asset_standards.cip25_metadata as {
            description: string;
            image: string;
            mediaType: string;
            name: string;
            type: string;
          };

          setClubTokens((prev) => [
            ...prev,
            {
              name: metadata.name,
              type: metadata.type,
            },
          ]);

          console.log({
            name: metadata.name,
            type: metadata.type,
          })
        }
      }
    };

    fetchClubs();
  }, [connected]);

  return (
    <WalletContext.Provider
      value={{
        club_tokens: clubTokens,
        saveClubTokens: (tokens) => setClubTokens(tokens),
      }}
    >
      <div className="">
        <div className="bg-[#282828] flex py-6 px-16 items-center justify-between">
          <div className="text-xl">CardaClub</div>
          <CardanoWallet isDark={true} />
        </div>
        {children}
      </div>
    </WalletContext.Provider>
  );
};
