# pandoc-sidenote

> Convert Pandoc Markdown-style footnotes into sidenotes

This is a simple [Pandoc filter] to convert footnotes into a format that can be
consumed by [Tufte CSS] and [Pandoc Markdown CSS Theme].

## Lua filter vs JSON filter

This project provides two, equivalent filters:

- `pandoc-sidenote.lua`, a Lua filter, which is run invoked internal to the
  pandoc binary. See [Lua filters] in the Pandoc docs for more.

  This is the preferred way to use `pandoc-sidenote`.

  The Lua filter is newer than the JSON filter, but I have tested it on a wide
  range of Markdown files and it produces byte-wise identical results.

- `pandoc-sidenote`, a [JSON filter][Pandoc filter] which reads a JSON
  representation of the pandoc AST on stdin and produces JSON on stdout.

  This filter is written in Haskell and compiled to a native executable. It uses
  the upstream `pandoc-types` library to deserialize the JSON it receives on
  stdin, which means that `pandoc-sidenote` can only be used with a version of
  `pandoc` built against the same `pandoc-types` version. See [JSON filter
  dependencies](#json-filter-dependencies) below.

  This is cumbersome, as it means that `pandoc-sidenote` needs to be rebuilt and
  republished for any new `pandoc` versions which makes a change to the core
  AST, even if it's a part of the AST that this filter doesn't care about.

## JSON filter dependencies

The Lua filter has no dependencies. Skip ahead to [Installation](#installation)
to install the Lua filter.

The JSON filter is built against a specific version of Pandoc. This table maps
`pandoc` versions to `pandoc-sidenote` versions:

| pandoc    | pandoc-sidenote        |
| ------    | ---------------        |
| 3.0       | 0.23.0                 |
| 2.11      | 0.22.0, 0.22.1, 0.22.2 |
| 2.9       | 0.20.0                 |
| 2.1, 1.19 | 0.19.0                 |
| 1.18      | 0.9.0                  |

If a newer version of `pandoc` has been released, the Stack build manifest
will need to be adjusted for that version, and the project then rebuilt.

## Installation

The Lua filter (`pandoc-sidenote.lua`) is the preferred way to use this project.
You can install it globally or local to a project. The Lua filter is a single
Lua file that works on all platforms where `pandoc` works.

Alternatively, you can install the `pandoc-sidenote` native executable, which
works as a JSON filter. JSON filters are slower and more tedious to use with
cutting edge Pandoc versions.

### Lua filter, globally installed

Consult the docs for how Pandoc [searches for Lua filters][option-lua-filter].

To install globally:

1.  Download `pandoc-sidenote.lua`

    → [pandoc-sidenote.lua]

2.  Put the file in the Pandoc user data directory. See the [Pandoc
    docs][option-lua-filter] for what the data directory is on your system.
    Specify a data directory by passing `--data-dir` when invoking `pandoc`.

3.  Invoke `pandoc` like this:

    ```
    pandoc --lua-filter pandoc-sidenote.lua [...]
    ```

As an example, on macOS or Linux, this shell snippet will download and install
the filter globally:

```bash
curl --remote-name --no-clobber --create-dirs \
  --output-dir "${XDG_DATA_DIR:-$HOME/.local/share/}/pandoc/filters" \
  -sSL "https://github.com/jez/pandoc-sidenote/raw/refs/heads/master/pandoc-sidenote.lua"
```

### Lua filter, locally installed

Consult the docs for how Pandoc [searches for Lua filters][option-lua-filter].

To install locally:

1.  Download `pandoc-sidenote.lua`

    → [pandoc-sidenote.lua]

2.  Put the file in the Pandoc user data directory. See the [Pandoc
    docs][option-lua-filter] for what the data directory is on your system.
    Specify a data directory by passing `--data-dir` when invoking `pandoc`.

3.  Invoke `pandoc` like this:

    ```
    pandoc --lua-filter ./pandoc-sidenote.lua [...]
    ```

As an example, on macOS or Linux, this shell snippet will download and install
the filter into the current directory:

```bash
curl --remote-name --no-clobber \
  -sSL "https://github.com/jez/pandoc-sidenote/raw/refs/heads/master/pandoc-sidenote.lua"
```

### JSON filter, Cabal

`pandoc-sidenote` is on Hackage and can thus be installed using `cabal`:

```bash
cabal install pandoc-sidenote
```

### JSON filter, Homebrew

If you're on OS X, you can install the `pandoc-sidenote` binary from my Homebrew
tap:

```bash
brew install jez/formulae/pandoc-sidenote
```

### JSON filter, From Source

Otherwise, you'll have to install from source. This project is written in
Haskell and built using [Stack]. If you're new to Haskell, now's a perfect time
to wet your toes! Go [install Stack first], then run these commands:

```bash
git clone https://github.com/jez/pandoc-sidenote

cd pandoc-sidenote

# this is going to be reaaally long the first time
stack build

# copy the compiled binary onto your PATH
stack install
```

## Haskell Library

The core functionality is also exposed as a Haskell library, which can be called
by Haskell applications such as [Hakyll]. It comes in two different flavours:

- [SideNote.hs](src/Text/Pandoc/SideNote.hs): An implementation making use of
  pandoc's native `Span` constructors. This is what's used in the
  `pandoc-sidenote` executable.

- [SideNoteHTML.hs](src/Text/Pandoc/SideNoteHTML.hs): An implementation that
  converts the footnote directly into HTML, enabling the embedding of arbitrary
  blocks inside of side and marginnotes.

On the whole, each file weighs in at just about 100 lines of code—check them out
if you're curious how they work.

## Notes to myself

Side note: I run this command to generate the zip files attached to releases
that are downloaded by the Homebrew formula:

```bash
make
```

It would be nice to get GitHub Actions set up to build and publish releases
for each tagged commit automatically.

I run this command to publish packages to Hackage:

```bash
# First, edit `package.yaml` to remove `-Werror`, then:

stack upload .
```


## License

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://jez.io/MIT-LICENSE.txt)

[Tufte CSS]: https://edwardtufte.github.io/tufte-css/
[Stack]: https://docs.haskellstack.org/en/stable/README/
[install Stack first]: https://docs.haskellstack.org/en/stable/README/
[Pandoc filter]: https://pandoc.org/filters.html
[Lua filters]: https://pandoc.org/lua-filters.html
[Tufte Pandoc CSS]: https://github.com/jez/tufte-pandoc-css
[Pandoc Markdown CSS Theme]: https://github.com/jez/pandoc-markdown-css-theme
[Hakyll]: https://jaspervdj.be/hakyll/
[pandoc-sidenote.lua]: https://raw.githubusercontent.com/jez/pandoc-sidenote/refs/heads/master/pandoc-sidenote.lua
[option-lua-filter]: https://pandoc.org/MANUAL.html#option--lua-filter
