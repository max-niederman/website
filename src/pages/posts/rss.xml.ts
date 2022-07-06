import rss from "@astrojs/rss";

const posts = Object.values(import.meta.globEager("./**/*.md"));

export const get = () =>
  rss({
    // `<title>` field in output xml
    title: "Max Niederman’s Blog",
    // `<description>` field in output xml
    description: "Essays on programming, math, and more.",
    // base URL for RSS <item> links
    // SITE will use "site" from your project's astro.config.
    site: import.meta.env.SITE,
    // list of `<item>`s in output xml
    // simple example: generate items for every md file in /src/pages
    // see "Generating items" section for required frontmatter and advanced use cases
    items: posts.map((post) => ({
      link: post.url,
      title: post.frontmatter.title,
      pubDate: post.frontmatter.published,
    })),
    // (optional) inject custom xml
    customData: `<language>en-us</language>`,
  });
