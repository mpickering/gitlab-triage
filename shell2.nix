let
  np = import <nixpkgs> {};
in
  np.mkShell { buildInputs = [ np.haskell.packages.ghc862.ghc
                               np.haskell.packages.ghc862.cabal-install
                               np.icdiff
                               np.gist
                               np.zlib
                               np.gmp
                               np.ncurses ];
              shellHook = ''
                export LD_LIBRARY_PATH=${np.gmp}/lib:${np.zlib}/lib:${np.ncurses}/lib
                ''; }
