---
title: "ttyper: a Terminal-Based Typing Test"
description: "A typing test on the command-line."
author: "Max Niederman"
published: "2021-03-14"
tags:
  - programming
  - command-line
  - tui
  - rust
---

TL;DR: [here](https://github.com/max-niederman/ttyper)'s the project on GitHub and it's installable as `ttyper` from Crates.io.

## Motivation

I was already pretty happy with the typing test I used to use, [monkeytype](https://monkeytype.com); however, I'm a big fan of CLI tools, so I wanted to be able to practice my typing without reaching for a browser or even a native application. I searched around for terminal-based typing tests, but none of them were quite what I had in mind.

Since there weren't any pre-existing tools which met my needs, I thought it would be a good opportunity to learning about [TUIs](https://en.wikipedia.org/wiki/Text-based_user_interface)[^1] to make one myself. I decided to use Rust with [tui-rs](https://github.com/fdehau/tui-rs), after being inspired by tools built with it such as [gitui](https://github.com/extrawurst/gitui), [bandwhich](https://github.com/imsnif/bandwhich), and [diskonaut](https://github.com/imsnif/diskonaut).

[^1]: As the Wikipedia article notes, 'TUI' can stand either for "text-based user interface" or for "terminal user interface." I use the latter here because tui-rs uses it and I personally prefer it.

## Design

I decided to go with a fairly simple, tried-and-true design: a list of words and an input box where you type each word one-by-one. I wanted an experience similar to [typings.gg](https://typings.gg/) or [10fastfingers](https://10fastfingers.com), but with the ability to return to a previous word after submission.

Here's a short recording of the current interface for the test input:
![Test TUI](https://github.com/max-niederman/ttyper/raw/main/resources/recording.gif)

## Calculating Metrics

Calculating the results of the test proved far harder than I had originally expected, although this may be due in part to the fact that I wanted more insights into my typing performance, such as keywise accuracy and WPM data, as well as a WPM graph.

First of all, I represented the test, more or less, as such:

```rust
struct Event {
    time: std::time::Instant,
    key: termion::Event::Key,
    correct: Option<bool>,
}

struct Word {
    text: String,
    events: Vec<Event>,
}

struct Test {
    words: Vec<Word>,
}
```

_You can read the real code [here](https://github.com/max-niederman/ttyper/blob/main/src/test/mod.rs)._

In this way, each "event" (keypress) can have a correctness value. Then, to find the accuracy, we can divide the number of correct events by the total number of events with correctness.

We can also find the WPM per each event from its time and its predecessor's time.

There's still a lot of things I want to change with ttyper's results calculations and UI, like adding a WPM over time graph and fixing weirdness with the overall WPM calculations, but it's working well enough for someone like me, who doesn't care too much about the exact WPM as long as I'm improving.

## Test Contents

For the test contents, I added an optional argument to specify a file to read, which is then split at whitespace and carriage returns. Then, I added functionality for automatically generating test content. It works by reading a language file and populating a list with a certain number of random lines from that file. Currently, `english100`, `english200`, and `english1000` are installed automatically. In the future, I'd like to add weighted language files based on n-gram analysis to make the test contents more realistic.

## Try It Out

If you want to try ttyper out for yourself, you can install it with Cargo:

```bash
cargo install ttyper
```

Alternatively, you can download pre-built binaries on [GitHub releases](https://github.com/max-niederman/ttyper/releases).
