import rss from "@astrojs/rss";

const posts = Object.values(import.meta.globEager("./**/*.mdx"));

export const get = () =>
  rss({
    title: "Max Niederman’s Blog",
    description: "Essays on programming, math, and more.",
    site: import.meta.env.SITE,
    items: posts.map((post) => ({
      link: post.url,
      title: post.frontmatter.title,
      pubDate: post.frontmatter.published,
    })),
    customData: `<language>en-us</language>`,
  });
