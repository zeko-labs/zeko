import { Link, Paper, Stack, Typography } from "@mui/material";
import {
  Transaction,
  TransactionStatus,
  WrappingDirection,
} from "../utils/types";
import { formatMina } from "../utils/format";
import { getMinaExplorerTxUrl, getZekoExplorerTxUrl } from "../utils/helper";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";
import useStoredTransactions from "../hooks/useGetStoredTransactions";
import Grid from "@mui/material/Unstable_Grid2/Grid2";

function TransactionItem({ tx }: { tx: Transaction }) {
  return (
    <>
      <Grid xs={2} sx={{ display: "flex", justifyContent: "start" }}>
        <Typography
          variant="caption"
          sx={{
            alignSelf: "center",
            color:
              tx.status === TransactionStatus.COMPLETE
                ? "green"
                : tx.status === TransactionStatus.ERROR
                ? "red"
                : "inherit",
          }}
        >
          {tx.status}
        </Typography>
      </Grid>
      <Grid xs={2} sx={{ display: "flex", justifyContent: "center" }}>
        <Typography>
          {tx.direction === WrappingDirection.WRAP ? "Wrap" : "Unwrap"}{" "}
        </Typography>
      </Grid>
      <Grid xs={4} sx={{ display: "flex", justifyContent: "end" }}>
        <Typography>{formatMina(BigInt(tx.amount))} MINA</Typography>
      </Grid>
      <Grid xs={4} sx={{ display: "flex", justifyContent: "end" }}>
        <Link
          href={
            tx.direction === WrappingDirection.WRAP
              ? getMinaExplorerTxUrl(tx.id)
              : getZekoExplorerTxUrl(tx.id)
          }
          target="_blank"
        >
          <Stack
            sx={{
              flexDirection: "row",
              alignItems: "center",
            }}
          >
            <Typography>{tx.id}</Typography>
            <OpenInNewIcon sx={{ height: 16 }} />
          </Stack>
        </Link>
      </Grid>
    </>
  );
}

export default function TransactionHistory() {
  const { data } = useStoredTransactions();
  if (!data || data.length === 0) {
    return <Typography>Your transaction history will appear here.</Typography>;
  }
  const txns = [...data].reverse();
  return (
    <Paper sx={{ p: 2, width: "100%" }}>
      <Grid container spacing={2} alignContent={"center"}>
        {txns.map((txn) => (
          <TransactionItem tx={txn} key={txn.id} />
        ))}
      </Grid>
    </Paper>
  );
}
