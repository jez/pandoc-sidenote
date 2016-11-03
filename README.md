# pandoc-sidenote

> Convert Pandoc Markdown-style footnotes into sidenotes

This is a simple [Pandoc filter] to convert footnotes into a format that can be
consumed by [Tufte CSS]. On the whole, this project weighs in at well under 100
lines of code. Check out [Main.hs](src/Main.hs) if you're curious how it works.

It's used by calling `pandoc --filter pandoc-sidenote`. To see it in action, see
[Tufte Pandoc CSS], a project which uses it. In particular, take a look at the
Makefile included in that project.

## Installation

If you're on OS X, you can install the `pandoc-sidenote` binary from the
Releases tab above. Just download it and put it anywhere on your PATH. I like
using `~/bin`. I have this line in my `bashrc`:

```bash
export PATH="$PATH:~/bin"
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
