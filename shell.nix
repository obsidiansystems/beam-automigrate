let pkgs = (import ./.ci/nixpkgs.nix).nixos2003;
in
  pkgs.mkShell {
    name = "beam-automigrate";
    buildInputs = [
      pkgs.cabal-install
    ];
    inputsFrom = [
      (import ./release.nix).ghc865.env
    ];
  }
