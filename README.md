# ghast
GitHub Actions STatus CLI tool

## Latest hits

- got basic auth working
- hit Actions "list runs" endpoint
- some helpers around request builders and parameters
- figure out what info I want to pull out of Actions
- get latest Jobs based on top Run
- build a data structure for those too
- URL-encode parameter values

## Next up
- ReaderT for credentials

## Medium term
- pull repository info
  - owner (e.g. MercuryTechnologies)
  - repo (e.g. mercury-web-backend)
  - current branch (e.g. matto/rul-79)
- get latest run, jobs in current branch
- list failed jobs
- download logs from failed jobs
- figure out how to work nicely with String and Text
 - similar for datetimes

## Longer term
- oauth2 support
- command-line options
  - check a specified branch
  - check a specified run / job