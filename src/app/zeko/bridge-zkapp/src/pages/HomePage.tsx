import {
  Button,
  Card,
  CardContent,
  Container,
  Stack,
  TextField,
  Typography,
} from "@mui/material";
import ConnectionWrapper from "../components/ConnectionWrapper";
import { useAppStore } from "../configs/store";
import { formatMina, parseMina } from "../utils/format";
import { useEffect, useState } from "react";
import { WrappingDirection } from "../utils/types";
import SwapVertIcon from "@mui/icons-material/SwapVert";
import RocketLaunchIcon from "@mui/icons-material/RocketLaunch";
import RocketIcon from "@mui/icons-material/Rocket";
import useBridge from "../hooks/useBridge";
import TransactionHistory from "../components/TransactionHistory";
import PendingIcon from "@mui/icons-material/Pending";
import useClearStoredTransactions from "../hooks/useClearStoredTransactions";

export default function HomePage() {
  const [account] = useAppStore((state) => [state.account]);
  const [amount, setAmount] = useState("");
  const [amountBN, setAmountBN] = useState(0n);
  const [targetAddress, setTargetAddress] = useState("");
  const [direction, setDirection] = useState(WrappingDirection.WRAP);
  const { mutate: startBridge, isPending } = useBridge();
  const { mutate: clearStoredTxns } = useClearStoredTransactions();

  const balanceBN = account?.balance.toBigInt();

  useEffect(() => {
    if (account) {
      setTargetAddress(account.publicKey.toBase58());
    }
  }, [account]);

  function handleAmountInputChanged(value: string) {
    let newValue = amount;
    try {
      setAmountBN(parseMina(value as any));
      newValue = value;
    } catch (e) {}
    setAmount(newValue);
  }

  function handleTargetAddressInputChanged(value: string) {
    setTargetAddress(value);
  }

  function handleSwitchDirectionButtonClick() {
    setDirection(
      direction === WrappingDirection.WRAP
        ? WrappingDirection.UNWRAP
        : WrappingDirection.WRAP
    );
  }

  async function handleBridgeButtonClick() {
    if (!account) {
      return;
    }
    startBridge({
      amount: amountBN,
      direction,
      sender: account.publicKey,
      target: targetAddress,
    });
    // todo: mechanism that is waiting for tx confirmation and will edit stored txn status via editStoredTransaction
  }

  const amountInputError =
    amountBN <= 0n
      ? "Amount must be positive"
      : balanceBN != null && amountBN > balanceBN
      ? "Amount exceeds balance"
      : undefined;

  const targetAddressInputError =
    targetAddress === ""
      ? "Target address must be specified"
      : targetAddress.length !== 55
      ? "Address is not in the correct format"
      : undefined;

  const formError = !!amountInputError || !!targetAddressInputError;

  const minaBalance = formatMina(balanceBN) ?? "-";
  const zekoBalance = "-";

  return (
    <>
      <Container>
        <Stack sx={{ mt: 4, alignItems: "center" }}>
          <ConnectionWrapper>
            <Stack
              sx={{
                gap: 4,
                alignItems: "center",
                width: "100%",
                maxWidth: 600,
              }}
            >
              <Stack
                sx={{
                  alignItems: "center",
                  gap: 2,
                  width: "100%",
                }}
              >
                <Stack sx={{ width: "100%" }}>
                  <Typography>From:</Typography>
                  <Card>
                    <CardContent>
                      <Stack sx={{ gap: 1 }}>
                        <Stack
                          sx={{
                            flexDirection: "row",
                            justifyContent: "space-between",
                            alignItems: "center",
                          }}
                        >
                          <Typography variant="h6">
                            {direction === "WRAP" ? "Mina" : "Zeko"}
                          </Typography>

                          <Typography variant="caption">
                            Balance:{" "}
                            {direction === WrappingDirection.WRAP
                              ? minaBalance
                              : zekoBalance}
                          </Typography>
                        </Stack>
                        <TextField
                          label="Amount"
                          value={amount}
                          onChange={(e) =>
                            handleAmountInputChanged(e.target.value)
                          }
                          error={amount !== "" && !!amountInputError}
                          helperText={amount !== "" && amountInputError}
                        ></TextField>
                      </Stack>
                    </CardContent>
                  </Card>
                </Stack>
                <Button
                  onClick={handleSwitchDirectionButtonClick}
                  startIcon={<SwapVertIcon />}
                >
                  Switch direction
                </Button>
                <Stack sx={{ width: "100%" }}>
                  <Typography>To:</Typography>
                  <Card>
                    <CardContent>
                      <Stack sx={{ gap: 1 }}>
                        <Stack
                          sx={{
                            flexDirection: "row",
                            justifyContent: "space-between",
                            alignItems: "center",
                          }}
                        >
                          <Typography variant="h6">
                            {direction === "WRAP" ? "Zeko" : "Mina"}
                          </Typography>
                        </Stack>
                        <TextField
                          label="Target address"
                          value={targetAddress}
                          onChange={(e) =>
                            handleTargetAddressInputChanged(e.target.value)
                          }
                          error={!!targetAddressInputError}
                          helperText={targetAddressInputError}
                        ></TextField>
                      </Stack>
                    </CardContent>
                  </Card>
                </Stack>
                <Button
                  variant="contained"
                  onClick={handleBridgeButtonClick}
                  disabled={isPending || formError}
                  startIcon={
                    isPending ? (
                      <PendingIcon />
                    ) : formError ? (
                      <RocketIcon />
                    ) : (
                      <RocketLaunchIcon />
                    )
                  }
                >
                  {isPending ? "Awaiting confirmation..." : "Bridge"}
                </Button>
              </Stack>
              <Stack sx={{ gap: 2, alignItems: "center", width: "100%" }}>
                <Typography variant="h3">Transaction history</Typography>
                <TransactionHistory />
                <Button
                  variant="text"
                  onClick={() => {
                    clearStoredTxns();
                  }}
                >
                  Clear transaction history
                </Button>
              </Stack>
            </Stack>
          </ConnectionWrapper>
        </Stack>
      </Container>
    </>
  );
}
