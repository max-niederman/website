import { defineConfig } from "astro/config";

import svelte from "@astrojs/svelte";
import mdx from "@astrojs/mdx";
import icon from "astro-icon";
import remarkMath from "remark-math";
import rehypeKatex from "rehype-katex";
import { remarkReadingTime } from "./src/markdown/reading-time.mjs";
import { remarkHeadingTree } from "./src/markdown/heading-tree.mjs";

// https://astro.build/config
export default defineConfig({
	integrations: [svelte(), mdx(), icon()],
	markdown: {
		remarkPlugins: [remarkMath, remarkReadingTime, remarkHeadingTree],
		rehypePlugins: [rehypeKatex],
		shikiConfig: {
			theme: "catppuccin-macchiato",
		},
	},
});
