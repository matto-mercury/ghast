# September 23-24 Hackathon proposal

We have a Hackathon coming up, and if I'm going to ask other people to join, I
feel like I ought to have some sort of a roadmap ready.

## The goal

Imagine me happily coding away. I get to a point where I'm more or less done
and push a commit. Now it's off in build-pipeline limbo somewhere. Instead of
opening a web browser, I flip over to my terminal and run `ghast`

```bash
% ghast
MercuryTechnologies/mercury-web-backend : matto/thx-1138
Run Waiting
```

I do some more stuff, maybe flip through a PR, and tab back to the shell to see
what's going on:

```bash
% ghast
MercuryTechnologies/mercury-web-backend : matto/thx-1138
Run InProgress
```

Woot! I go back to that PR, then check back in a few minutes:

```bash
% ghast
MercuryTechnologies/mercury-web-backend : matto/thx-1138
Run Failed
- Failed Jobs:
  - Build and test mercury-web-backend with werror
  Code compiled with warnings
```

Oh, dammit. I didn't do a `make ci` before committing, just builds. I could
check the warnings locally, but the Actions job already did that, so I run
`ghast` again in verbose mode:

```bash
% ghast -v
MercuryTechnologies/mercury-web-backend : matto/thx-1138
Run Failed
- Failed Jobs:
  - Build and test mercury-web-backend with werror
  (a list of ghc warning text follows)
```

## The path

Right now I can get the full build logs from a failed job in a GHA run, as
text. My first thought was to just filter on all instances of the string
`error: `, which will more or less work but produces some false positives (e.g.
where we compile or test error-processing code in mwb). The sick idea I had,
though, was to parse the logs for the kinds of errors we tend to run into,
since they usually have fairly consistent patterns.

We don't need to drill in too far, just enough to recognize the type of the
error (for example, "fix your migrations" is pretty clear, as are ghc errors) so
we can classify them as e.g.  "didn't update golden types", or "test failed", or
whatever and ideally parse out the error text to display (if prompted).

### First step: enumerate build errors

Before we can parse build errors we have to figure out what they are, right? So
the first step is to enumerate common build errors and get some example text for
each.

### Second step: parse them

It shouldn't be astonishingly hard to build attoparsec parsers for these things,
right? We don't have to be _perfect_, we just have to be able to identify them.
Ideally we'd provide enough of the error message to tell the user what happened,
and I don't think that'd be hard, but if it is we can leave it for later.