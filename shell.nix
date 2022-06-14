with (import ./.ci/nixpkgs.nix).nixos2003.pkgs;
let
  ghc = haskell.packages.ghc865.ghcWithPackages
    (pkgs : with pkgs; [ cabal-install ghcid ]);
in
  stdenv.mkDerivation {
    name = "dev-beam-migrate";
    buildInputs = [ ghc zlib xz postgresql96 ncurses pkgconfig ghcid ];
    shellHook = ''
      eval $(grep export ${ghc}/bin/ghc)
      export LD_LIBRARY_PATH="${zlib}/lib:${xz.out}/lib:${ncurses}/lib";
    '';
  }
