let pkgs = (import ./.ci/nixpkgs.nix).nixos2003;
in
  pkgs.mkShell {
    name = "beam-automigrate";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      (import ./release.nix).compilers.ghc8107.env
    ];
  }
