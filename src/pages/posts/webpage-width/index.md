---
title: "On the Width of Webpages"
author: "Max Niederman"
description: "A rant on webpages wider than their viewports."
published: "2022-07-13"
tags:
  - web-development
  - design
layout: "$/layouts/Post.astro"
setup: |
  import MobileExample from "./MobileExample.astro"
---

Websites should never be wider than their viewports. I think that most people understand this, but it's incredible how often I come across sites which get this wrong. It seems that every other site I read on my phone looks like this:

<figure>
  <MobileExample />
  <figcaption>A flawed but common mobile layout.</figcaption>
</figure>

An actual phone, of course, cannot simply render the content outside of the screen. Instead, browsers handle overflowing by allowing users to move the viewport by scrolling.

This is a good default for _horizontal_ overflow, but it's usually bad for vertical overflow. When mobile users scroll vertically, it is easy to accidentally scroll horizontally as well, which can be extremely distracting from the content.

Most web developers test their sites primarily on desktop browsers, where the viewport is much wider, so it's easy not to notice the issue. Even when developers test using desktop browsers' mobile emulation tools, they typically use a mouse or touchpad, so they are less likely to accidentally scroll horizontally.

Web developers should adopt the following priniples:

- Whenever possible, the design should respond to the viewport's width rather than rely on users to scroll.
- Failing that, large elements should scroll _individually_, using `overflow-x: scroll`, rather than force the entire body to widen.
- Websites should be tested in an environment as close as possible to the one in which they will be used.
