# Argos

<p align="center">
  <img width="512" src="https://previews.123rf.com/images/ververidis/ververidis1902/ververidis190200181/117571543-aerial-view-of-larisa-castle-in-argos-city-at-peloponnese-peninsula-greece.jpg" />
</p>

`argos` is a command line tool to programatically generate an autocompletion script from a tree like-structure of arbitrary depth. For instance, this is how you'd describe (albeit just a tiny little bit) git with argos:
```argos
command(status)
command(push) {
    option(long(force), short(f)),
    option(long(all)),
    option(long(tags))
}
command(remote) {
    command(add) {
        option(long(fetch), short(f)),
        option(long(tags))
    },
    command(prune) {
        option(long(dry-run), (n))
    }
    option(long(help), short(h))
}
```

This will yield an autocompletion script so that when typing `git`, the available autocomplete will be `status`, `push` and `remote`. Furthermore, completing one of these will lock out the other options:
```bash
$ git remote <TAB><TAB>
add        prune        --help        -h

$ git remote add <TAB><TAB>
--fetch        -f        --tags
```

## Usage

An installer script is provided in this repo. Simply run `./install.sh` after cloning to compile the binaries and install the program in your machine. Once that's done, to generate your autocompletion script with argos run `argos compile <PROGNAME> -s <SOURCE> -t <TARGET>` and move your new file to `/etc/bash_completion.d/`.

By convention, argos files should end with the extension `.argos`, but there are no requirements over that.
