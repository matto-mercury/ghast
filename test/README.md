# test/ README

`ghast`'s tests aren't super well-organized right now, and don't have a ton of
coverage. Mostly this is because I've been building it through the repl in a
fairly exploratory way. Other complicating factors are lack of stubs for calls
to Github's API, and the sheer volume of structured text that needs to be
parsed and traversed (which is at odds with a tight, legible unit test suite).
This last bit may be how I get into lenses.

One thing that's helped is `Test.Hspec.Attoparsec`, which I discovered after
writing explicit tests for many of the lower-level parsers, and I want to go
back through and rewrite those.