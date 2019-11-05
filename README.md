# Code it later #

## Why ##

I already have other versions:

+ [code_it_later_ml](https://github.com/ccqpein/code_it_later_ml) ocaml version
+ [code-it-later](https://github.com/ccqpein/code-it-later) clojure version

so why I write this version. Because after I finish first ocaml version, I thought why not haskell. Then I start this repo. 

After this version, I find something:

**small input:**
haskell faster than ocaml (just a little), and clojure version is slow. 

**big input:**
clojure is fastest, haskell is faster than ocaml.

===================================

**update from 1/27/2019**

I make this version become concurrency version. And tested with same input:

* clojure version: cost 2.2 second, ~200% cpu cost.
* ocaml version: cost 2.2 second, 100% cpu cost.
* this haskell version: cost 0.6 seconds, 140% cpu cost.

I am satisfied about this result. And change haskell code to concurrency is not that hard.

**update from 11/3/2019**

after ghc 8.10 add `-xn` to active new GC in runtime.

## Install ##

`code-it-later-hs.cabal` including all build configuration which `cabal` need.

use `cabal new-build` after installed all packages in `build-depends` 

```shell 
cabal new-build
```

after build, use additional script to enable concurrency

```shell
cp ./codeitlater /usr/local/bin/ ;
ln -sfv $(pwd)/dist-newstyle/build/x86_64-osx/ghc-8.6.5/code-it-later-hs-0.2.1.0/x/codeitlater.core/build/codeitlater.core/codeitlater.core /usr/local/bin/codeitlater.core
```

## Usage ##

As same as clojure version, so please check [clojure version](https://github.com/ccqpein/code-it-later)

Features those still developing:

- [ ] output to org file
- [x] expand other languages json file

### Features only in this version ###

`-dx {folder_name}` or `--ignore-dir {folder_name}` will ignore all files in `{folder_name}`

## Next ##

~~ocaml looks like not very convenient for using parallel. I will try parallel in this haskell version for practice.~~

I do not use parallel, I make it concurrency with mutil-core, and it is good enough. 
