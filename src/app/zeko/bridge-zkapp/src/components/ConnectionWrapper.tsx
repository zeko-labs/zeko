import { Link, Stack, Typography } from "@mui/material";
import { PropsWithChildren, useEffect, useState } from "react";
import { auroLink } from "../utils/constants";
import { useAppStore } from "../configs/store";
import ConnectWallet from "./ConnectWallet";
import useConnectWallet from "../hooks/useConnectWallet";
import { useEffectOnce, useInterval } from "usehooks-ts";
import LoaderStack from "./LoaderStack";

export default function ConnectionWrapper({ children }: PropsWithChildren) {
  const [userPublicKey, account, workerClient, setAccount, initWorkerClient] =
    useAppStore((state) => [
      state.userPublicKey,
      state.account,
      state.workerClient,
      state.setAccount,
      state.initWorkerClient,
    ]);
  const [accountExists, setAccountExists] = useState<boolean>();
  const { connectWallet } = useConnectWallet();

  async function checkFeePayer() {
    if (!userPublicKey || !workerClient) {
      return;
    }
    if (!account) {
      setAccountExists(undefined);
      console.log(
        `Checking if fee payer account ${userPublicKey.toBase58()} exists...`
      );
      const res = await workerClient!.fetchAccount({
        publicKey: userPublicKey,
      });
      const doesAccountExist = res.error == null;
      if (doesAccountExist) {
        setAccount(res.account);
      }
      setAccountExists(doesAccountExist);
    }
  }

  useEffectOnce(() => {
    (async () => {
      await initWorkerClient();
      await connectWallet();
    })();
    window.mina?.on("accountsChanged", (accounts: string[]) => {
      connectWallet();
    });
    return () => {
      window.mina?.off("accountsChanged");
    };
  });

  useEffect(() => {
    checkFeePayer();
  }, [userPublicKey, !!workerClient]);

  // Interval check so that user does not have to refresh site after depositing into account
  useInterval(async () => {
    checkFeePayer();
  }, 5000);

  if (!window.mina) {
    return (
      <Stack sx={{ gap: 2, alignItems: "center" }}>
        <Typography>Could not find a wallet.</Typography>
        <Link href={auroLink} target="_blank">
          Install Auro wallet here
        </Link>
      </Stack>
    );
  }

  if (!workerClient) {
    return (
      <LoaderStack>
        <Typography>Initializing...</Typography>
      </LoaderStack>
    );
  }
  if (!userPublicKey) {
    return <ConnectWallet />;
  }
  if (!account) {
    if (accountExists == null) {
      return (
        <LoaderStack>
          <Typography>Checking if fee payer account exists...</Typography>
        </LoaderStack>
      );
    } else if (accountExists === false) {
      return (
        <Stack sx={{ alignItems: "center", gap: 2 }}>
          <Typography>Account does not exist.</Typography>
          <Link
            href={`https://faucet.minaprotocol.com/?address=${userPublicKey.toBase58()}`}
            target="_blank"
            rel="noreferrer"
          >
            Visit the faucet to fund this fee payer account.
          </Link>
        </Stack>
      );
    }
  }

  return <>{children}</>;
}
