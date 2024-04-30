import { CircularProgress, Stack } from "@mui/material";
import { PropsWithChildren } from "react";

export default function LoaderStack({ children }: PropsWithChildren) {
  return (
    <Stack sx={{ flexDirection: "row", gap: 2, alignItems: "center" }}>
      <CircularProgress />
      {children}
    </Stack>
  );
}
