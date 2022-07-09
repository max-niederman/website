---
title: "You Probably Don't Need a VPN"
author: "Max Niederman"
description: "A brief explanation of what VPNs are useful for, and why most people don't need one."
published: "2022-07-08"
tags:
  - internet
  - privacy
layout: "$/layouts/Post.astro"
---

If you're reading this, there's a good chance that you've seen at least on VPN advertisement before. Almost all of them claim to improve "privacy" and "security" for their users. These claims _can_ be true, but they are intentionally misleading.

For the vast majority of users, a VPN is useless. All a VPN does is tunnel your internet traffic through their network, hiding it from your ISP and masking your IP address from its destination. But most traffic you use is _already_ encrypted with HTTPS, so in these cases a VPN makes no difference.

It does _not_, however, protect your privacy from websites that you use. Even if you hide your IP address, they can still fingerprint your browser or use any number of methods to deanonymize you.

For the vast majority of users, there are only two reasons to use a VPN:

- You want to spoof your location. This can be useful for some sites with per-country licensing, but most of these sites will try to block VPN users.
- You want to "hide" your torrent traffic from your ISP. I say "hide" very loosely here because your ISP will still _know_ that you're torrenting, they just won't bother you because they have plausible deniability.

And obviously, a VPN will do **nothing** to hide you from governments. Almost all VPNs keep (and possibly sell) logs of your traffic, and the government can and will subpoena those logs. You should be very wary, even if a VPN _claims_ to keep no logs.

Of course, even if your VPN really does keep no logs, a government would still easily be able to deanonymize you some other way, unless you are _extremely_ careful. If you're interested in protecting your privacy from motivated governments, I recommend that you give up, particularly if you live in a [Five Eyes](https://en.wikipedia.org/wiki/Five_Eyes) country.
