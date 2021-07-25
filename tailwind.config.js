const colors = require("tailwindcss/colors");
const { fontFamily } = require("tailwindcss/defaultTheme");

module.exports = {
  mode: "jit",
	purge: ["public/**/*.html", "src/**/*.{astro,tsx,svelte}"],
	darkMode: "media",
	theme: {
		colors: {
			transparent: "transparent",
			current: "currentColor",
			black: "black",
			white: "white",
		},
		extend: {
      minHeight: {
        "screen": "100vh",
      },
      minWidth: {
        "screen": "100vw",
      },
			fontFamily: {
				serif: ["EB Garamond", ...fontFamily.serif],
        mono: ["Fira Code", ...fontFamily.mono],
			},
			fontSize: {
				"10xl": "12rem",
				"11xl": "16rem",
			},
    },
  },	
};
