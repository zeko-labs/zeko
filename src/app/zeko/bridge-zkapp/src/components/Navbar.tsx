import { AppBar, Container, Stack, Toolbar, Typography } from "@mui/material";
import ConnectWallet from "./ConnectWallet";

export default function Navbar() {
  return (
    <>
      <AppBar sx={{ bgcolor: "#030909" }}>
        <Container>
          <Toolbar sx={{ gap: 2 }}>
            <Typography
              variant="button"
              sx={(theme) => ({ color: theme.palette.primary.main })}
            >
              Zeko bridge
            </Typography>
            <Stack sx={{ flexGrow: 1 }} />
            <ConnectWallet />
          </Toolbar>
        </Container>
      </AppBar>
      <Toolbar />
    </>
  );
}
