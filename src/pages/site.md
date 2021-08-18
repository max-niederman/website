---
title: "About This Website"
description: "A description of this website's purpose and its technical details."
published: "2021-08-11"
updated: "2021-08-18"
status: "in progress"
tags:
- meta
- web-development
layout: "$/layouts/Default.astro"
---

# Purpose

This website has three main purposes:

- as a place to publish my writings where they can be viewed and linked to by anyone. For this reason it must:
  - Be hosted in a highly-available manner.
  - Be usable without JavaScript and with other uncommon browsers/configurations.
  - Be accessible to those with color-blindness, using screen-readers, etc.
  - Load quickly even on non-ideal Internet connections.
- as a part of my writing workflow. I write in Github-flavored Markdown with some other extensions, mostly from Pandoc. While writing, I like to see the Markdown source as well as the final typeset document. To this end it must:
  - Support a superset of the Markdown features I require.
  - Update to reflect changes within a couple seconds of a change in the source.
  - Have a portable development environment so that I can write from any device easily.
- as a long-lasting archive for my writings. The content published to this website should survive as long as possible. It must:
  - Be compatible with archiving services such as [the Internet Archive](https://archive.org).
  - Be readable as pure HTML. There should be no dependence even on CSS.
  - Institute measures to mitigate [link rot](https://en.wikipedia.org/wiki/Link_rot).

# Design

This website's design is *heavily* inspired by that of [Gwern.net](https://gwern.net). Currently it's more or less a copy lacking a few features such as

- Sidenotes
- Link popups
- Tables of contents

In the future, I hope to change this by implementing some of those features as well as altering the design so as to be more distinctive.

## Fonts

Currently, EB Garamond is used for everything except codeblocks, which use Fira Code. In the future, this will probably change. If it doesn't prove prohibitively time-consuming, I may even work on my own fonts for this website.

# Technical

The source code is all public and can be viewed [here](https://github.com/max-niederman/website) on [my GitHub](https://github.com/max-niederman).

## Static Site Generation

This website is statically generated using a variety of tools. Chief among them is [astro](https://astro.build). It uses an "islands architecture" to enable true components with completely opt-in JavaScript. Currently, it enables this website to use *zero* client-side JavaScript.

The Markdown documents which comprise the vast majority of this website are processed using the [unified](https://unifiedjs.com) content-processing software suite and styled using [Sass](https://sass-lang.com).

Remark Plugins:
- `remark-gfm` for Github-flavored Markdown features.
- `remark-slug` for heading anchors.
- `remark-math` to create math nodes using inline Markdown.
- `remark-footnotes` to make footnotes.
- `remark-gemoji` for emoji shortcode transformation.
- `@silvenon/remark-smartypants` for smart punctuation.

Rehype Plugins:
- `rehype-mathjax` to render the aforementioned math nodes.

## Hosting

Currently, the website is hosted on [Cloudflare Pages](https://pages.cloudflare.com). This has numerous benefits:

- Unlimited, free bandwidth
- Low latency
- HTTP/3 over QUIC
