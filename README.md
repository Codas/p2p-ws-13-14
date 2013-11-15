p2p-ws-13-14
============

## Windows Dev
Everyone who uses the command line should really ditch cmd.exe and use an
alternative like [Console2](http://sourceforge.net/projects/console/).

## Git
For everyone who does not want to use the command line version (but you should!
:wink:), or does not like the eclipse intergration (do NOT use that one xD), I
can recommend [Github for Windows](http://windows.github.com/) or
[Tortoise Git](https://code.google.com/p/tortoisegit/).

## Haskell
in order to get the server / Haskell version to run you need a dev. version of the
[threepenny GUI library](https://github.com/HeinrichApfelmus/threepenny-gui/tree/develop).

Assuming you have git, haskell ghc and cabal up and running (via the
[Haskell Platform](http://www.haskell.org/platform/) for example), only 6 steps
are needed.

1. (in temp dir:) `git clone https://github.com/HeinrichApfelmus/threepenny-gui.git`
2. `cd threepenny-gui`
3. `git checkout origin/develop`
4. `cabal install`
5. in haskell project dir: `cabal install --only-dependencies`
6. in haskell project src dir: `runhaskell Server.hs`

Subsequently, only the last (on last two on updated dependencies) step is needed
to start the server.

**Good Haskell resources**:

- [Sublime Haskell](https://github.com/SublimeHaskell/SublimeHaskell) for
  [Sublime Text Editor](http://www.sublimetext.com/) of course, Editor
- [EclipseFP](http://eclipsefp.github.io/), IDE

## go version
Get go here: http://golang.org/doc/install#windows

To compile, use `go build <file>`

## Protocol
For the protocol discussion see [protocol draft](protocol.md)
