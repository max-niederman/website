export default {
  buildOptions: {
    site: "http://maxniederman.com", // Your public domain, e.g.: https://my-site.dev/. Used to generate sitemaps and canonical URLs.
    sitemap: true, // Generate sitemap (set to "false" to disable)
  },
  renderers: ["@astrojs/renderer-react", "@astrojs/renderer-svelte"],
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
