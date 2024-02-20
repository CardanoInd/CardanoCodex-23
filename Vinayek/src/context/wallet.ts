import { createContext } from "react";

export type WalletContextType = {
  club_tokens: {
    name: string;
    type: string;
  }[];
  saveClubTokens: (
    tokens: {
      name: string;
      type: string;
    }[]
  ) => void;
};

export const WalletContext = createContext<WalletContextType | null>(null);
