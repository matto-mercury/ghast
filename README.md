# ghast
GitHub Actions STatus CLI tool

## User guide

`ghast` is intended to show you the status of your Github Actions job, and
eventually helpful information about why it failed (if indeed it did), from the
comfort of your terminal.

### Building

Builds with `cabal`. Sorry, `stack` fans.

### Authenticating

`ghast` pulls credentials out of environment variables, for now. You'll need a
[github personal access token](https://github.com/settings/tokens) with `repo`
and `workflow` permissions. (These usually expire at a set time.)

Put your github username in `$GITHUB_USER` and your token in `$GITHUB_KEY`,
either in your shell's `.rc` file or in a separate shell script you source when
you want to use `ghast`. I know, it's a pain, I'm sorry.

Github's `gh` CLI uses oauth2, I plan/hope to implement that eventually (see
"roadmap, ish" below).

### Using

`ghast` pulls git remote and current branch data out of `git`. From a repo with
Github Actions jobs set up, you can run `ghast` from the command line and get a
reasonable response for the currently checked-out branch.

#### `--branch` selects a specific branch

If you want to check a different branch, maybe because you bounced to a second
project while the first was in CI, use `--branch your-branch-name`. `ghast` will
display the branch it's trying to check on but assumes it can trust you, so if
(for example) you yubikey the branch param it'll check for jobs on a branch
starting with `ccccc` and helpfully tell you that there's no GHA runs for it.

#### `--rawlogs` splats all the logs to stdout

If you want to get the raw, unparsed logs for a failed run, use `--rawlogs`.
Probably pipe the output to a file if you do.

#### `--thisrun` selects a specific Github Actions run

`ghast` normally pulls down info for the most recent Github Actions run on the
current branch (or whatever you select with `--branch`), which is probably your
most recent commit.

If you're hacking on `ghast` and want to run it against a specific Github
Actions run, use `--thisrun 12345678` (with the actual Actions run you want,
obvs). You can find this ID in the URL when you're poking around the Github
Actions web UI, or with `gh run list` (and friends) from the CLI if you have the
`gh` tool installed (that's github's CLI tool).

> If you're not hacking on `ghast` I don't know why you'd use this switch, but
> it's there if you want it.

## Roadmap, ish

### Latest hits

- pull repository info
  - owner (e.g. MercuryTechnologies)
  - repo (e.g. mercury-web-backend)
  - current branch (e.g. matto/rul-79)
- get latest run, jobs in current branch
- list failed jobs
- download logs from failed jobs
- command-line options
  - check a specified branch
- check a specified run / job
- parse ghc errors out of logs

### Next features
- parse more errors out of logs
  - test failures
  - "fix your migrations"
  - golden types don't match
  - ...?
- different verbosity levels

### Tech debt
- "Render pretty" functions scattered all over, mostly Repl and GithubLogs
- "steps" logic all in Repl
- repl test fixture crap in Repl (corpora) and AppEnvironment (framework)
- parsers could use names to improve attoparsec errors

### Medium term
- figure out how to work nicely with datetimes, if needed

### Longer term
- oauth2 support