# pandoc-sidenote

> Convert Pandoc Markdown-style footnotes into sidenotes

This is a simple [Pandoc filter] to convert footnotes into a format that can be
consumed by [Tufte CSS]. On the whole, this project weighs in at well under 100
lines of code. Check out [Main.hs](Main.hs) if you're curious how it works.

It's used by calling `pandoc --filter pandoc-sidenote`. To see it in action, see
[Tufte Pandoc CSS], a project which uses it. In particular, take a look at the
Makefile included in that project.

The core functionality is also exposed as a library, which can be called by Haskell
applications such as Hakyll.

## Dependencies

`pandoc-sidenote` is built against Pandoc version 1.18. You'll need to upgrade
to this version. If a newer version has been released, the Stack build manifest
will need to be adjusted for that version, and the project then rebuilt.

## Installation

If you're on OS X, you can install the `pandoc-sidenote` binary from my Homebrew
tap:

```
brew install jez/formulae/pandoc-sidenote
```

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

## License

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://jez.io/MIT-LICENSE.txt)

[Tufte CSS]: https://edwardtufte.github.io/tufte-css/
[install Stack first]: https://docs.haskellstack.org/en/stable/README/
[Pandoc filter]: http://pandoc.org/scripting.html#json-filters
[Tufte Pandoc CSS]: https://github.com/jez/tufte-pandoc-css
