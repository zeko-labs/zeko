import { createTheme, responsiveFontSizes } from "@mui/material/styles";

export let theme = createTheme({
  palette: {
    mode: "dark",
  },
  typography: {
    fontFamily: '"Inter", sans-serif',
    button: { fontWeight: 600 },
  },
  components: {
    MuiToolbar: {
      styleOverrides: {
        root: {
          paddingLeft: 0,
          paddingRight: 0,
        },
      },
    },
  },
});
theme = responsiveFontSizes(theme);
