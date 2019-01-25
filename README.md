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

ocaml looks like not very convenient for using parallel. I will try parallel in this haskell version for practice.
