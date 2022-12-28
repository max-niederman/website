import { defineConfig } from "astro/config";
import mdx from "@astrojs/mdx";
import remarkGfm from "remark-gfm";
import remarkGemoji from "remark-gemoji";
import remarkSlug from "remark-slug";
import remarkMath from "remark-math";
import remarkSmartypants from "remark-smartypants";
import remarkFootnotes from "remark-footnotes";
import rehypeKatex from "rehype-katex";

// https://astro.build/config
export default defineConfig({
  site: process.env.SITE ?? "https://maxniederman.com",
  markdown: {
    remarkPlugins: [
      remarkGfm,
      remarkGemoji,
      remarkSlug,
      remarkMath,
      remarkSmartypants,
      remarkFootnotes,
    ],
    rehypePlugins: [
      [
        rehypeKatex,
        {
          output: "html",
          trust: true,
        },
      ],
    ],
    syntaxHighlight: "shiki",
    shikiConfig: {
      theme: "one-dark-pro",
    },
  },
  integrations: [mdx()],
});
