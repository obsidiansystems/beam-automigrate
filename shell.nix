let pkgs = (import ./.ci/nixpkgs.nix).unstable;
in
  pkgs.mkShell {
    name = "beam-automigrate";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      (import ./release.nix).compilers.ghc9103.env
    ];
  }
