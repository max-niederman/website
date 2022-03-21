// Full Astro Configuration API Documentation:
// https://docs.astro.build/reference/configuration-reference

import autoprefixer from 'autoprefixer';

// @ts-check
export default /** @type {import('astro').AstroUserConfig} */ ({
	buildOptions: {
		site: process.env.SITE ?? "https://maxniederman.com",
	},
	plugins: [autoprefixer],
	markdownOptions: {
		render: [
			"@astrojs/markdown-remark",
			{
				remarkPlugins: [
					"remark-gfm",
					"remark-gemoji",
					"remark-slug",
					"remark-math",
					"@silvenon/remark-smartypants",
					"remark-footnotes",
				],
				rehypePlugins: [
					"rehype-mathjax",
				],

				syntaxHighlight: "shiki",
				shikiConfig: {
					theme: "dracula",
				}
			}
		],
	},
});
