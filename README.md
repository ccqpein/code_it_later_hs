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

## Install ##

`code-it-later-hs.cabal` including all build configuration which `cabal` need.

use `cabal build` after installed all packages in `build-depends`

after build, use additional script to enable concurrency

```shell
cp ./codeitlater /usr/local/bin/ ;
ln -sfv $(pwd)/dist/build/codeitlater.core/codeitlater.core /usr/local/bin/codeitlater.core
```

## Usage ##

As same as clojure version, so please check [clojure version](https://github.com/ccqpein/code-it-later)

Features those still developing:

- [ ] output to org file
- [ ] expand other languages json file

## Next ##

~~ocaml looks like not very convenient for using parallel. I will try parallel in this haskell version for practice.~~

I do not use parallel, I make it concurrency with mutil-core, and it is good enough. 
