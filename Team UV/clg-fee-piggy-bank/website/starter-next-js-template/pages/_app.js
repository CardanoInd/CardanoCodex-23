import "../styles/globals.css";
import { MeshProvider } from "@meshsdk/react";

function MyApp({ Component, pageProps }) {
  return (
    <MeshProvider>
      <Component {...pageProps} />
    </MeshProvider>
  );
}

export default MyApp;
