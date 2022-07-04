import { defineConfig } from "astro/config";

export default defineConfig({
	site: process.env.SITE ?? "https://maxniederman.com",
	markdown: {
		remarkPlugins: [
			"remark-gfm",
			"remark-gemoji",
			"remark-slug",
			"remark-math",
			"remark-smartypants",
			"remark-footnotes",
		],
		rehypePlugins: [
			["rehype-katex", {
				output: "html",
				trust: true,
			}],
		],

		syntaxHighlight: "shiki",
		shikiConfig: {
			theme: "one-dark-pro",
		}
	},
});
