# About

This is the source code for [my blog](http://www.prickyourfinger.org).
The compiled output can be found on [another branch](https://github.com/berkson/berkson.github.io/tree/master).

# Compiling

To compile, do:

```
cabal sandbox init
cabal install cabal-install
cabal install --only-dep```

This will install Hakyll and other dependencies.
Now just run

```
make install
```
