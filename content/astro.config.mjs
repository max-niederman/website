export default {
  buildOptions: {
    site: "http://maxniederman.com",
    sitemap: true,
  },
  renderers: [],
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
          { default: (await import("rehype-epigraph")).default }, // No idea why we have to do this but it doesn't work otherwise
        ],
      }
    ],
  },
};
