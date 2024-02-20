import "@/styles/globals.css";
import type { AppProps } from "next/app";
import { MeshProvider } from "@meshsdk/react";
import { Layout } from "@/components/AppLayout";

export default function App({ Component, pageProps }: AppProps) {
  return <MeshProvider>
    <Layout>
      <Component {...pageProps} />
    </Layout>
  </MeshProvider>;
}
