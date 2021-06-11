Argos
===

<p align="center">
  <img width="512" src="https://previews.123rf.com/images/ververidis/ververidis1902/ververidis190200181/117571543-aerial-view-of-larisa-castle-in-argos-city-at-peloponnese-peninsula-greece.jpg" />
</p>

# Contents
- [Argos](#argos)
- [Contents](#contents)
  - [Introduction](#introduction)
  - [Installation and Usage](#installation-and-usage)
  - [Syntax](#syntax)
  - [How does it work?](#how-does-it-work)

## Introduction

`argos` is a command line tool to programatically generate an autocompletion script from a tree-like structure of arbitrary depth. 

## Installation and Usage

Argos is provided both as a command-line tool as well as a Haskell library. To install the CLI, clone the repo and compile the program with the installer `install.sh`. Once it's installed, you can compile an argos file into an autocompletion script by running `argos compile my-program -s my-argos-file.argos` and moving the generated `my-program-completion.bash` to `/etc/bash_completion.d/`.

As for the Haskell library, you can install it with either `cabal install argos` or `stack install argos`, and the following imports:
```hs
import Argos
```

## Syntax

Argos uses a very simple language but expressive language to describe the program arguments' order. For instance, let's take a look at [argos' structure](argos.argos):
```argos
command(compile) {
  option(long(help), short(h)),
  option(long(source), short(s), argument(files(.*[.]argos)))
}

command(complete) {
  option(long(help), short(h)),
  option(long(options), short(o))
}

option(long(help), short(h))
```

This will yield an autocompletion script so that when typing `argos`, the available autocomplete will be `compile`, `complete`, `-h` and `--help`. Furthermore, completing one of these will lock out the other options:
```bash
$ argos <TAB> <TAB>
compile        complete        --help        -h

$ git compile <TAB> <TAB>
--help        -h        --source        -s
```

Argos makes a distinction between commands and options, each denoted by their respective keyword. Commands only need its name, and can then be expanded upon with their options and sub-commands inside braces. As options commonly have a long and short version, they are specified in the `option` keyword. The `long` is required, but the `short` isn't.

Moreover, options can receive an optional `argument`. This can be one of `directories` or `files`. The first one will return all directories in current directory when the option is completed and autocomplete is prompted:
```argos
command(example) {
  option(long(dir), argument(directories)),
}
```

```bash
$ l
bin/       tests/
documents/ workspace/
downloads/

$ example --dir <TAB><TAB>
bin/
documents/
downloads/
tests/
workspace/

$ example --dir do<TAB><TAB>
documents/
downloads/
```

Meanwhile, `files` can receive an optional regex to filter files. If no regex is supplied, all files are listed:
```argos
command(example) {
  options(long(files), argument(files)),
  options(long(argos-files), arguments(files(.*[.]argos)))
}
```

```bash
$ l
bar       foo
bar.argos foo.argos
baz.argos qux

$ example --files <TAB><TAB>
bar
bar.argos
baz.argos
foo
foo.argos
qux

$ example --files ba<TAB><TAB>
bar
bar.argos
baz.argos

$ example --argos-files <TAB><TAB>
bar.argos
baz.argos
foo.argos

$ example --argos-files ba<TAB><TAB>
bar.argos
baz.argos
```

Inside a command, each option or subcommand should be separated by `,`. Whitespace is not required, but should be used to improve readability. **Trailing commas are not supported** (WIP).

Top-level commands or options are, instead, separated by newline. Empty commands (those that do not have neither commands nor options) can have either empty braces or no braces at all.

## How does it work?

When you run `argos compile`, a bash completion script is generated with the following template:
```bash
#!/bin/bash

_${progName}_completion()
{
    local cur len
    cur=${COMP_WORDS[COMP_CWORD]}
    ARGS="${COMP_WORDS[@]}"
    
    RES=$(argos complete ${progName} --options "$ARGS")
    COMPREPLY=($(compgen -W "$RES" -- "$cur"))
}

complete -F _${progName}_completion ${progName}
```

At the same time, the argos file is parsed and a simplified version of it is saved as `~/.config/argos/progName.data`. This file is then read whenever `argos complete progName` is called. Argos then traverses the tree with the passed options, and upon reaching its deepest levels returns all possible autocompletions.
