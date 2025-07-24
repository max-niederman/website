import { defineConfig } from "astro/config";

import svelte from "@astrojs/svelte";
import mdx from "@astrojs/mdx";
import icon from "astro-icon";
import remarkMath from "remark-math";
import rehypeTypst from "@myriaddreamin/rehype-typst";
import { remarkTypedLinks } from "./src/markdown/links.mjs";
import { remarkReadingTime } from "./src/markdown/reading-time.mjs";
import { remarkHeadingTree } from "./src/markdown/heading-tree.mjs";

// https://astro.build/config
export default defineConfig({
	integrations: [svelte(), mdx(), icon()],
	markdown: {
		remarkPlugins: [remarkMath, remarkTypedLinks, remarkReadingTime, remarkHeadingTree],
		rehypePlugins: [rehypeTypst],
		shikiConfig: {
			theme: "catppuccin-macchiato",
		},
	},
});
