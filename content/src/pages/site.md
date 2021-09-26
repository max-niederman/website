---
title: "About This Website"
description: "A description of this website's purpose and its technical details."
published: "2021-08-11"
updated: "2021-08-18"
status: "updated"
tags:
  - meta
  - web-development
layout: "$/layouts/Default.astro"
---

## Purpose

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

## Design

### Slugs

Each slug segment must contain only numbers, lowercase Latin letters, and hyphens exclusively for word seperation. Segment length should be kept to a minimum, but never at the cost of expressiveness.

By default, a page is located by only one slug segment. Additional slug segments are used only when the following conditions are met:

- Many pages (present or planned) share one essential, simple theme.
- This theme should not be complex, nor should it be in any way esoteric.

### Post Metadata

#### Tags

Tags on this website are of two kinds:

- **Literal Tags** are tags which describe the page itself. For instance, this page has the [`meta` tag](/tags/meta). The page itself _is_ meta, as opposed to being about or otherwise related to things which are meta.
- **Subject Tags** are tags which describe the subject of the page. For instance, one of this page's _subjects_ is the development of this website, wherefore it has the [`web-development` tag](/tags/web-development).

Currently, this distinction is not represented in any way by the UI, but this is a planned feature. Often, but not always, literal tags will be adjectives and subject tags nouns.

Tag names follow the same convention as slugs on this site: all lowercase and hyphenation for word seperation.

#### Dates

The publication date I believe is self-explanatory; it's the date when the page was first live on this website.

The update date is the date of the last _substantive_ change to the page. This does not include changes which fix typoes or other trivial mistakes.

#### Status

The status tag is used for the "completeness" of the page. Currently, the following statuses are used:

- "draft" refers to a rough draft.
- "mature draft" refers to a mature draft.
- "updated" refers to a page which is at the moment complete, but will become incomplete in the future. For instance, the [Changelog](/changelog) will never be "complete," only "updated."
- "complete" refers to a page which is complete and will always, bar modification, be so.

### User Interface

This website's design is _very_ inspired by that of [Gwern.net](https://gwern.net). Currently it's more or less a copy lacking a few features such as

- Sidenotes
- Link popups
- Tables of contents

In the future, I hope to change this by implementing some of those features as well as altering the design so as to be more distinctive.

#### Fonts

Currently, EB Garamond is used for everything except codeblocks, which use Fira Code. In the future, this will probably change. If it doesn't prove prohibitively time-consuming, I may even work on my own fonts for this website.

## Technical

The source code is all public and can be viewed [here](https://github.com/max-niederman/website) on [my GitHub](https://github.com/max-niederman).

### Static Site Generation

This website is statically generated using a variety of tools. Chief among them is [astro](https://astro.build). It uses an "islands architecture" to enable true components with completely opt-in JavaScript. Currently, it enables this website to use _zero_ client-side JavaScript.

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

### Hosting

Currently, the website is hosted on [Cloudflare Pages](https://pages.cloudflare.com). This has numerous benefits:

- Unlimited, free bandwidth
- Low latency
- Support for HTTP/3 over QUIC
