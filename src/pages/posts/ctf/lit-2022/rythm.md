---
title: "LIT CTF 2022: Rythm's Double Identity"
author: "Max Niederman"
description: 'Writeup of the "Rythm''s Double Identity" challenge from the LIT CTF 2022.'
published: "2022-07-22"
tags:
  - hacking
  - ctf
  - ctf/lit
layout: "$/layouts/Post.astro"
---

> It is known that Rythm is an unparalleled prodigy at competitive programming. Anybody who dares challenge instantly pulverizes into mere dust. But just as the old adage goes, 无敌是多么的寂寞~ [something like the English idiom "it's lonely at the top"]. After days of constantly getting **_infinite_** positive rating changes on the Competitive Programming platform CodeForces as Superj6, he decides to adopt a secret double identity. She was conceived in Rythm's own image, and the only person who could compete with Rythm.
>
> It is known that Rythm competed in the majority of CodeForces contests starting from July 2019, but sometimes as his second identity. Find his 2nd most used account for contests, and wrap the username (case-sensitive) in `LITCTF{}` to obtain the flag

One invariant which I guessed would hold true is that Rythm never competes in contests with multiple accounts. This means that I could eliminate any accounts which have competed against Rythm's main account, 'Superj6'.

CodeForces has a public API documented [here](https://codeforces.com/apiHelp). I used Python with the `requests` module to scrape a list of contests and their standings, which includes all the participating accounts:

```python
import requests

# fetch a list of all contests
contest_list = requests.get("https://codeforces.com/api/contest.list").json()["result"]
# filter for finished contests taking place after July 2019
applicable_contest_list = list(filter(
    lambda contest: contest["phase"] == "FINISHED"
    and contest["startTimeSeconds"] >= 1561939200,
    contest_list,
))

contests = []
for i, contest in enumerate(applicable_contest_list):
    # fetch standings
    res = requests.get(
        "https://codeforces.com/api/contest.standings?contestId="
        + str(contest["id"])
    ).json()

    if res["status"] == "OK":
        # collect a set of all accounts in this contest
        participants = set()
        for row in res["result"]["rows"]:
            for party_member in row["party"]["members"]:
                participants.add(party_member["handle"])

        contests.append({ 
          "contest": contest, 
          "participants": participants
        })
    else:
        print("error scraping standings: {}".format(res["comment"]))
```

Then, I generated a multiset of all the accounts, where the multiplicity is the number of competitions in which 'Superj6' has not competed but the account has:

```python
from collections import Counter

KNOWN_HANDLE = "SuperJ6"

candidates = Counter()

for contest in contests:
    if KNOWN_HANDLE not in contest["participants"]:
        for participant in contest["participants"]:
            candidates[participant] += 1
```

Next, I removed all candidates who have competed against Rythm's main account:

```python
for contest in contests:
    if KNOWN_HANDLE in contest["participants"]:
        for participant in contest["participants"]:
            del candidates[participant]
```

Then, I just printed and checked the top 5 candidates manually:

```python
print(candidates.most_common(5))
```