export default {
  buildOptions: {
    site: "http://maxniederman.com",
    sitemap: true,
  },
  renderers: [],
  markdownOptions: {
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
      import("rehype-epigraph"), // No idea why we have to `import` it but it doesn't work otherwise
    ],
  },
};
