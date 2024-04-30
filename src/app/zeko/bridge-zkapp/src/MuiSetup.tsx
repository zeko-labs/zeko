"use client";

import { CssBaseline } from "@mui/material";
import type { ReactNode } from "react";

import { theme } from "./configs/theme";
import { ThemeProvider } from "@mui/material/styles";

type Props = {
  children: ReactNode;
};

export const MuiSetup = ({ children }: Props) => {
  return (
    <>
      <ThemeProvider theme={theme}>
        <CssBaseline />
        {children}
      </ThemeProvider>
    </>
  );
};
