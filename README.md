# ghast
GitHub Actions STatus CLI tool

## Latest hits

- got basic auth working
- hit Actions "list runs" endpoint
- some helpers around request builders and parameters

## Next up

- figure out what info I want to pull out of Actions
- get latest Jobs based on top Run
- build a data structure for those too

## Medium term
- pull repository info
  - owner (e.g. MercuryTechnologies)
  - repo (e.g. mercury-web-backend)
  - current branch (e.g. matto/rul-79)
- URL-encode parameter values
- list failed jobs
- download logs from failed jobs

## Longer term
- oauth2 support