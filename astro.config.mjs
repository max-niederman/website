export default {
  buildOptions: {
    site: "http://maxniederman.com",
    sitemap: true,
  },
  renderers: ["@astrojs/renderer-svelte"],
  markdownOptions: {
    remarkPlugins: [
      "remark-lint",
      "remark-gfm",
      "remark-slug",
      "remark-math",
      "remark-footnotes",
      "@silvenon/remark-smartypants",
      "remark-gemoji",
    ],
    rehypePlugins: ["rehype-mathjax"],
  },
};
