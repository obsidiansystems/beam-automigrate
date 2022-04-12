{ ghc ? "ghc865" }:
let
  ghcOverride = (import ./.ci/ghc-beam-overrides.nix).${ghc};
in with ghcOverride.pkgs;
  stdenv.mkDerivation {
    name = "dev-beam-migrate";
    buildInputs = [
      cabal-install
      (ghcOverride.exe)
      zlib
      xz
      postgresql96
      ncurses
      pkgconfig
    ];
    shellHook = ''
      eval $(grep export ${ghcOverride.exe}/bin/ghc)
      export LD_LIBRARY_PATH="${zlib}/lib:${xz.out}/lib:${ncurses}/lib";
    '';
  }
