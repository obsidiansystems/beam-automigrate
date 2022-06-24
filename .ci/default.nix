{ }:
let
  nixpkgsSets = import ./nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck;
  beam-overrides = self: super: (import ./gargoyle { haskellPackages = self; }) // {
    beam-core = self.callHackageDirect {
      pkg = "beam-core";
      ver = "0.9.0.0";
      sha256 = "sha256:0d79ca1rxnq2hg1ap7mx3l3qg3hwfaby4g3cckk4y3ml86asw6jh";
    } {};
    beam-migrate = self.callHackageDirect {
      pkg = "beam-migrate";
      ver = "0.5.0.0";
      sha256 = "sha256:14bhbxk5sap0iymwqwva0j1szg63bs721j9srg0agz11g7gi1fsw";
    } {};
    beam-postgres = dontCheck (self.callHackageDirect {
      pkg = "beam-postgres";
      ver = "0.5.0.0";
      sha256 = "sha256:1lh9rb0gs50myffy0dsr7na5x8g8hq88rxsh0hi9rvvlvk3vf8cq";
    } {});
    which = self.callHackageDirect {
      pkg = "which";
      ver = "0.2";
      sha256 = "1g795yq36n7c6ycs7c0799c3cw78ad0cya6lj4x08m0xnfx98znn";
    } {};
  };
  ghcs = rec {
    ghc865 = nixos2003.haskell.packages.ghc865.override {
      overrides = beam-overrides;
    };
    ghc884 = nixos2003.haskell.packages.ghc884.override {
      overrides = beam-overrides;
    };
    ghc8107 = unstable.haskell.packages.ghc8107.override {
      overrides = beam-overrides;
    };
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "beam-automigrate" ../. {}) ghcs
