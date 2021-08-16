---
title: "Automatic Theming"
description: "Software themes are an important part of any system, but traditionally a lot of work is necessary to theme a system consistently, especially with the increasing prevalence of webapps; to make it worse yet, this work must be repeated every time one desires a new aesthetic. Automatic theming tools are still young, but with some initial effort, they can enable theme switching over an entire system in seconds."
published: "2021-08-14"
updated: "2021-08-14"
status: "draft"
tags:
  - aesthetics
  - theming
  - linux
  - programming
layout: "$/layouts/Default.astro"
---

There is a fundamental lack of consistency in the aesthetics of software. With a few exceptions, each and every application uses its own themes because there is no standard. The best we can hope for is vaguely similar dark and light themes.

Fortunately, with great effort, it is possible to achieve a relatively high degree of consistency. It may not be possible to theme every single website you will ever visit, but you can theme your terminal, your desktop environment, your browser, your text editor, and far more. Many (including myself), have spent hours upon hours manually theming their systems.

This is not ideal; it takes a huge time commitment to do something like this. The worst part, in my opinion, is that once you've done this, you have to do it *again* if you want to change up the look of your system.

Of course, this obvious solution is just automate it, that is create a tool which accepts themes in a common format, adapts them to various applications, and applies the adapted themes to them; however, this is easier said than done. In order for a solution to be adopted, it must be able to theme a significant subset of its users' applications, and in order to do that, there must be users to contribute support for their applications.

For this reason, automatic theming has a very high barrier of entry. The only people who can achieve it are those with the skills to roll support for their applications themselves. Combined with the large time commitment and the fact that the benefits of even a completely consistent solution are small (primarily reduced context-switching).

# Pywal

By far the most-used automatic theming tool is [pywal](https://github.com/dylanaraps/pywal). It was created by Dylan Araps of Neofetch fame. Pywal's basic function is to extract a color palette from an image. After it extracts the palette, it sets open terminal colors with it and renders user-defined templates.

With a little work, this can be used to automatically theme almost all applications. When I was still using pywal, I wrote [a wrapper script](https://github.com/max-niederman/dots/blob/ad1bc832bcdc462f52c5abeff1cba0929d9691ae/fish/functions/walu.fish) which ran all the executables in [a directory](https://github.com/max-niederman/dots/tree/ad1bc832bcdc462f52c5abeff1cba0929d9691ae/wal/update.d). These executables would then use the rendered templates to apply themes to various apps which needed to be reloaded (or didn't support symlinked theme files).

Pywal is very simple, and because of this simplicity, it is quite easy to use. However, it is, first and foremost, a *palette extraction* tool, not a theming automation tool. This is apparent in its monolithic architecture: its palette extraction function cannot easily be decoupled from its theme application function, which is not even customizable.

A lacking theme application function is not *necessarily* a problem. For instance, as the UNIX philosophy dictates, we could use pywal only for palette extraction and another tool for theme application. This is, more or less, what I did myself when I was using pywal. Unfortunately no such tool currently exists, and even if it did that solution would only *raise* the barrier of entry.

## The Fatal Limitation

None of the preceding problems are, in my opinion, reason enough to abandon pywal and use a different tool for palette extraction. What I do believe warrants this is the simple fact that pywal extracts ugly themes.

Pywal's palette extraction works by a backend-frontend architecture. You can select from multiple backends. As of writing this, *all* of pywal's backends use a [k-means clustering]() algorithim to extract colors from an image.

K-means clustering is well suited for extracting one or two accents for use in a UI, but for most images it will produce bad *system* colorschemes. This is primarily becuase k-means clustering can only extract colors which are actually in the image[^1].

[^1]: Or the centroid of colors in the image, but it would be highly unusual for a color which isn't very near to one in the image to be found.

It is for this reason that pywal only uses a list of accents instead of the far more useful named colors (red, green, blue, etc.). You want a dangerous action to be colored red? Well, sorry, you can't; it's "accent #3" or nothing.

Even worse, the accents are often very similar colors which reduces contrast thereby making reading harder. This is only a big problem in images which are mostly one color, and it could probably be greatly improved by processing the colors for better contrast after the clustering step.

Because pywal is primarily focused on extracting colors from images, this limitation will always plague it, severely limiting its uses.
