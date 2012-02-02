# PlaySpace Online!

## Building

### Clone the *Shady* packages


This web application depends on two Haskell packages that are in active development:
[`shady-gen`](http://github.com/sseefried/shady-gen) and 
[`shady-graphics`](http://github.com/sseefried/shady-graphics). First clone these two repositories from the links before and, in both repositories, check out the `playspace-online` branch:

```bash
$ git checkout playspace-online
```

### Install <tt>cabal-dev</tt>

```bash
$ cabal install cabal-dev
```

### Build <tt>play-space-online</tt>

#### A short note on the <tt>.cabal</tt> file.

This web application depends on a *lot* of packages. Unfortunately, my experience is
that specifying the version numbers loosely leads to mixed results on many architectures.
Hence, every single package this application depends on is specified
*exactly* in the `.cabal` file. This is okay, since no other package will ever depend on this application.

Thus, it will only build with GHC version 6.12.3. You may have noticed a file called
`play-space-online.cabal.ghc-6.12.3`. In future there will be multiple `.cabal.<ghc-version>` files
like this which can copy over the real one to build with alternative versions of GHC. Until someone
fixes the Haskell Cabal, this is the solution I'm going with.

#### Build it! 

After you have checked out this repo:

```bash
$ cabal-dev add-source /path/to/shady-gen
$ cabal-dev add-source /path/to/shady-graphics
$ cabal-dev install --flags=production
```

### Building again: an important note on <tt>cabal-dev</tt>

Perhaps you will hack on package `shady-gen` or `shady-graphics` and want to rebuild 
*PlaySpace Online* afterwards. If you do this be sure to run these commands again:

```bash
$ cabal-dev add-source /path/to/shady-gen
$ cabal-dev add-source /path/to/shady-graphics
```

The utility `cabal-dev` tarballs up the packages at a particularly point in time. If you change the
sources `cabal-dev` has no way of knowing.

### Creating a <tt>.cabal</tt> file for other GHC versions

The way I discovered all the packages that *PlaySpace Online* depends on was to 

* build it successfully by tinkering a lot.
* Run `cabal install` with the `-v` flag and dump the output to a file.
* `grep` for all lines beginning with `selecting`.

Some `sed` munging later I had a very long list of dependencies which I copy and pasted into the
`.cabal` file.

If a contributor would like to do the same thing for other versions of GHC I'd be very grateful.
