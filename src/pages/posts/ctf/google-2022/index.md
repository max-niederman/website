---
title: "Google CTF 2022"
author: "Max Niederman"
description: "My writeups for Google CTF 2022."
published: "2022-07-04"
tags:
  - hacking
  - ctf
  - ctf/google
layout: "$/layouts/Post.astro"
setup: |
  import engraverAnimation from "./engraver-animation.gif"
---

A couple days ago, a few friends and I competed in the Google CTF. It was our first time doing a CTF, so we weren't sure how well we were going to do. Although we were only able to find four flags, I had a great time; there's something unrivaled about the feeling of finally cracking a puzzle you've been working on for hours.

We ended the competition in 76th place, putting us in the top 20% of teams that found at least one flag. All things considered, I'm very happy with that result and I'm excited to play more CTFs in the future.

I only wrote up three of the four flags, because I honestly don't fully understand the other one, JS Safe 4.0. All I know is that solving it involves a lot of horrifying JavaScript.

## treebox

Treebox is a simple sandbox escape challenge in Python. The flag is in the `flag` file in the working directory, and we had to escape the sandbox to read it.

The attacker can execute arbitrary Python programs, but only if they meet a condition checked by `verify_secure`, which just traverses the [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) and blocks execution if the program contains any `Import`, `ImportFrom`, or `Call` nodes.

For example, the following code would not be allowed because it contains `Call` nodes:

```python
print(open("flag").read())
```

So we needed a way to write function calls which don't _look_ like function calls from the parser's perspective.

There are lots of ways to do this, but the first one we thought of is to use operator overloading: in Python, objects can have special methods which override special language behavior, including arithmetic operations. For example, the `__add__`, `__sub__`, `__mul__`, and `__div__` methods are called when you do arithmetic with an object.

So we could define a class like this:

```python
class Functions:
    __add__ = open

funs = Functions()
fun + "flag" # returns a file descriptor
```

But we still needed an instance of the class to call the methods. This is where the second interesting trick comes in: `raise` statements can instantiate classes. If you call `raise MyException`, where `MyException` is a class, it will instantiate that class with no arguments. But in the AST, this is a `Raise` node, not a `Call` node like regular class instantiation.

```python
class Functions:
    __add__ = print
    __sub__ = iter
    __mul__ = open

try:
    raise Functions
except Functions as funs:
    # equivalent to `for block in iter(open("flag")):`
    for block in funs - (funs * "flag"):
        funs + block # equivalent to `print(block)`
```

And this successfully grabs the flag.

By the way, the iterator is just a way to read the file using only builtin functions. We couldn't call `f.read()` using operator overloading because `read` is a method on the file descriptor.

## legit

In Legit, the attacker has access to a CLI utility which downloads Git repositories and lists and displays files from them. Since the attacker controls the argument passed to `git clone`, they can completely control the contents of the Git repository.

The object of the challenge is to trick the CLI into reading a file outside of the Git repository you downloaded.

Before actually printing the file, it normalizes and resolves the path to make sure it's actually inside the Git repository:

```python
def show_file():
  filepath = input(">>> Path of the file to display: ")

  real_filepath = os.path.realpath(os.path.join(_REPO_DIR, filepath))
  if _REPO_DIR != os.path.commonpath((_REPO_DIR, real_filepath)):
    print("Hacker detected!")
    return

  result = subprocess.run(["cat", real_filepath], capture_output=True)
  print(result.stdout.decode())
```

So just making a symlink pointing to the flag in the Git repository doesn't work, because `show_file` would resolve it with `os.path.realpath` before checking if the file is in the repository.

Also, the fact that it uses `cat` to read the file made us very suspicious that symlinks are involved, since `cat` will resolve them a second time. We decided to look at the documentation for `os.path.join` and `os.path.realpath` to see if they have any unintuitive behavior.

And, as it turns out, they do! By default, `os.path.realpath` tries to resolve symlinks, but if resolution fails, it gives up and stops resolving symlinks in the rest of the path. There's a `strict` mode which raises the error up to the caller, but it's not used here.

Symlink resolution fails if a cycle is detected, so we created a repository with the following symlinks:

```
cycle => ./cycle
flag  => /flag
```

Then, we told the CLI to read the file at the path `./cycle/../flag`. While normalizing the path, it tried to resolve the `/cycle` segment, but failed and gave up. Then, it saw the `/..` and removed the previous segment, `/cycle`. Finally, it added on `/flag` to get the path `REPO_DIR/flag`. That got passed to `cat`, which resolved the symlink and read the flag.

If that explanation doesn't satisfy you, you can read the source code of CPython's `_joinrealpath`, which is the meat of `os.path.realpath`, [here](https://github.com/python/cpython/blob/cf1732619a61f7b7b5223ebaf6be6455d28257f2/Lib/posixpath.py#L400).

## engraver

Engraver is a challenge in the hardware category involving a robot arm holding a laser pointer. The attachment contains three things:

- A picture of a robot arm holding a laser pointer.
- A long-exposure shot of the robot arm drawing letters with the laser pointer.
- A dump of USB traffic to the arm containing movements which draw the flag.

Some research on the bot shown in the picture yielded[^1] a GitHub repository with [a reference](https://github.com/bharrisonb/LSC-6_Cmd_Photon/blob/master/LSC-6%20LewanSoul%20Communitcation%20Protocol%20of%20Servo%20Controller.pdf) for the protocol used by the arm's servos. The protocol appeared to match HID data in packets sent to the arm, so we exported the relevant packets to JSON from Wireshark and wrote a simple parser to get the data. We put it in [a Google Sheet](https://docs.google.com/spreadsheets/d/1o5WzEaTdthRJO7yFNi9ONgJI-nG2s2aFh-_y4KPyjPk/edit?usp=sharing) and graphed it to see if the data looked reasonable. Three servos were moving, two of them in about the same way. By guess-and-check, we figured out that the two similar servos were controlling the X and Y axes, and the third servo was switching the laser on and off.

[^1]: Annoyingly, we _also_ found a GitHub repository which used a different kind of servo with a different, but similar, control protocol. That ended up wasting us a couple hours of time.

At that point, we had all the data we needed to find the flag, but it wasn't really possible to read it in that format. We wrote a program in Rust to generate an animated GIF of the lines the arm would draw.

<img src={engraverAnimation} alt="animation of arm movement">

It required a bit of frame-by-frame analysis to read what the arm was writing, but we did figure it out eventually.
