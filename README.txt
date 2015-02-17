To get started:

 1) Install GHC https://github.com/ghcformacosx/ghc-dot-app
 2) cd beancounter
 3) cabal sandbox init
 4) cabal install --only-dependencies
 5) make coffee
 6) cabal install
 7) write TSV files
   a) first row is the header, bag name<tab>bag buyer<tab>cost
   b) subsequent rows are signout entries: initials<tab>grams
 8) **/beancounter(*) *.txt

