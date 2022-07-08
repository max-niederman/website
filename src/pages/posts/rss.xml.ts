import rss from "@astrojs/rss";

const posts = Object.values(import.meta.globEager("./**/*.md"));

export const get = () =>
  rss({
    title: "Max Niederman’s Blog",
    description: "Essays on programming, math, and more.",
    site: import.meta.env.SITE,
    stylesheet: "/rss/styles.xsl",
    items: posts.map((post) => ({
      link: post.url,
      title: post.frontmatter.title,
      pubDate: post.frontmatter.published,
      description: post.compiledContent(),
    })),
    customData: `<language>en-us</language>`,
  });
