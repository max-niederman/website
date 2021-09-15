export default {
  buildOptions: {
    site: "YOUR_SITE_URL",
    sitemap: true,
  },
  renderers: [],
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
