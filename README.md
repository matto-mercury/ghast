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
- ReaderT for credentials
- pull repository info
  - owner (e.g. MercuryTechnologies)
  - repo (e.g. mercury-web-backend)
  - current branch (e.g. matto/rul-79)
- get latest run, jobs in current branch
- list failed jobs

## Next up
- download logs from failed jobs
- parse errors out of logs

## Medium term
- figure out how to work nicely with datetimes
- command-line options
  - check a specified branch
  - check a specified run / job

## Longer term
- oauth2 support