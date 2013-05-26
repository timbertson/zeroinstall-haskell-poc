# ZeroInstall.hs - Proof of concept

This is not a Thing that Works. It's some code I wrote to investigate a Haskell-based implementation of ZeroInstall. It no longer compiles, because I haven't put the effort in to update it since I wrote it. This may go nowhere, but I was curious to try it out.

Interesting bits:

* src/GpgTest.hs verifies a feed's GPG signature, using the haskell `openpgp` package (i.e no dependency on gnupg binary or libs).

* src/Run.hs (and friends, built by zeroinstall.cabal in the root) will load and run a local selections document.


