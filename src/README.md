# `ghast` Source Documentation

## Organization

Most, maybe all, of the Github-specific stuff lives in `Github/`. Helpers for,
say, pulling a query for GHA runs out of a GHA job live in `Jobs.hs` - the
higher level, not the lower level, of the tree.

The error parsers live in `Parsers/`, as you'd expect. There are a couple
Attoparsec parsers in `AppEnvironment.hs` to parse `git` output.

`Repl.hs` is kind-of a thing that imports the world so I can mess around in
`ghci` but also a top-level place where things often get implemented, before
moving them out into their own modules. Currently the workflow chunks live
there.