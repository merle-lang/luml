# Luml

Luml is a type-inferred language in the tradition of Elm, OCaml, F# etc, 
which cross-compiles to Lua with the intention of being used as an
embedded scripted language, or stand-alone using the Lua interpreter
or LuaJit. It is implemented in OCaml.

This isn't intended for any production use and won't see any future development; it is left here as a historical curiosity and as an example of implementing a type-inferred language and cross compiler in OCaml.

## Installation

### macOS

`curl -Ls https://git.io/f4y8I | bash`

Test installation by running `luml` to access the REPL.
