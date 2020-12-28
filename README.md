# fcall
A call graph generator for haskell,displaying how functions reference and call each other.

# implementation
1 it parse the source to AST

2 make function definations as nodes,

3 and if a defination is reference in other defination,a edge is added to the call graph.

4 visualize this graph with a simple haskell webserver and d3.js

# files
There are two versions,"hsviewerGhc.hs" is based on ghc api which is more up to date,and "hsviewer.hs" is based on haskell-src-exts which is deprecated(maybe not the case due to the new release of this library)

# dependencies
scotty

ghc-exactprint

then run cabal install 

# run
```haskell
runhaskell hsviewerGhc.hs someHsFile.hs
```
then go to localhost:3000 to view the result
