import { defineConfig } from "astro/config";

import svelte from "@astrojs/svelte";
import mdx from "@astrojs/mdx";
import icon from "astro-icon";
import remarkMath from "remark-math";
import rehypeKatex from "rehype-katex";
import { remarkReadingTime } from "./src/markdown/reading-time.mjs";

// https://astro.build/config
export default defineConfig({
	integrations: [svelte(), mdx(), icon()],
	markdown: {
		remarkPlugins: [remarkMath, remarkReadingTime],
		rehypePlugins: [rehypeKatex],
		shikiConfig: {
			theme: "catppuccin-macchiato",
		},
	},
});
