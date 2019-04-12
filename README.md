# Haskell Playground

## Workflow
Since this is a playground, the purpose is usually not in building an executable, but in REPL. Issue
`stack ghci <source-name>` to load the source into a ghci instance; we can start playing around from there.

* `stack build` gets the required dependencies and builds the executable.
* The cabal file lists the dependencies that the sources in `src`.
* Define `extra-deps` in `stack.yaml`.
