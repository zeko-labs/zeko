module.exports = {
  content: ["./src/**/*.{html,js,ts,jsx,tsx,md,mdx}"],
  theme: {
    extend: {
      colors: {
        white: "#fff",
        orange: {
          600: "#ff9828",
          800: "#ff972881",
        },
        gray: {
          200: "rgba(255, 255, 255, 0.616)",
          600: "#232323",
          800: "#1a1a1c",
        },
        sky: {
          400: "#40a9ff",
        },
      },
      boxShadow: {
        default: "0px 4px 20px rgba(0, 0, 0, 0.25)",
      },
      keyframes: {
        fadeIn: {
          "0%": {
            opacity: 0,
          },
          "100%": {
            opacity: 1,
          },
        },
      },
      animation: {
        fadeIn: "fadeIn 0.5s ease-in-out",
      },
    },
  },
  plugins: [],
};
