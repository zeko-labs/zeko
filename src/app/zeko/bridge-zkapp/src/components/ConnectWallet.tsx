import { Button, Typography } from "@mui/material";
import { useAppStore } from "../configs/store";
import useConnectWallet from "../hooks/useConnectWallet";
import { auroLink } from "../utils/constants";
import { formatAddress } from "../utils/format";

export default function ConnectWallet() {
  const userPublicKey = useAppStore((state) => state.userPublicKey);
  const { connectWallet } = useConnectWallet();
  return !window.mina ? (
    <Button href={auroLink} target="_blank">
      Install wallet
    </Button>
  ) : userPublicKey ? (
    <Typography
      variant="button"
      sx={(theme) => ({
        color: theme.palette.primary.main,
        textTransform: "none",
      })}
    >
      Connected: {formatAddress(userPublicKey.toBase58())}
    </Typography>
  ) : (
    <Button onClick={connectWallet}>Connect wallet</Button>
  );
}
