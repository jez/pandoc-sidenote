# pandoc-sidenote

> Convert Pandoc Markdown-style footnotes into sidenotes

This is a simple [Pandoc filter] to convert footnotes into a format that can be
consumed by [Tufte CSS] and [Pandoc Markdown CSS Theme].

As a command line utility, the project may be used by calling `pandoc --filter
pandoc-sidenote`. To see it in action, see [Tufte Pandoc CSS], a project which
uses it. In particular, take a look at the Makefile included in that project.

Further, the core functionality is also exposed as a library, which can be
called by Haskell applications such as [Hakyll]. It comes in two different
flavours:

  - [SideNote.hs](src/Text/Pandoc/SideNote.hs): An implementation making use of
    pandoc's native `Span` constructors. This is what's used in the
    `pandoc-sidenote` executable.

  - [SideNoteHTML.hs](src/Text/Pandoc/SideNoteHTML.hs): An
    implementation that converts the footnote directly into HTML,
    enabling the embedding of arbitrary blocks inside of side and
    marginnotes.

On the whole, each file weighs in at just about 100 lines of code—check them out
if you're curious how they work.

## Dependencies

`pandoc-sidenote` is build against a specific version of Pandoc. This table maps
`pandoc` versions to `pandoc-sidenote` versions:

| pandoc    | pandoc-sidenote         |
| ------    | ---------------         |
| 3.0      | 0.23.0  |
| 2.11      | 0.22.0, 0.22.1, 0.22.2  |
| 2.9       | 0.20.0                  |
| 2.1, 1.19 | 0.19.0                  |
| 1.18      | 0.9.0                   |

If a newer version of `pandoc` has been released, the Stack build manifest
will need to be adjusted for that version, and the project then rebuilt.

## Installation

### Cabal

`pandoc-sidenote` is on Hackage and can thus be installed using `cabal`:

```bash
cabal install pandoc-sidenote
```

### Homebrew

If you're on OS X, you can install the `pandoc-sidenote` binary from my Homebrew
tap:

```bash
brew install jez/formulae/pandoc-sidenote
```

### From Source

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
[Pandoc filter]: http://pandoc.org/scripting.html#json-filters
[Tufte Pandoc CSS]: https://github.com/jez/tufte-pandoc-css
[Pandoc Markdown CSS Theme]: https://github.com/jez/pandoc-markdown-css-theme
[Hakyll]: https://jaspervdj.be/hakyll/
