import { MaestroClient, Configuration } from "@maestro-org/typescript-sdk";

export const maestroClient = new MaestroClient(
  new Configuration({
    apiKey: "8mIIOCPvbN7IzTLUqOVDLc4PbOhdWkd6",
    network: "Preprod",
  })
);
