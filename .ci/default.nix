{ }:
let
  pkgs = (import ./nixpkgs.nix).unstable;
  inherit (pkgs) lib;
  ghcs = {
    ghc9103 = pkgs.haskell.packages.ghc9103.override {
      overrides = self: _: {
        # Use the parser's Data instances instead of local orphan instances.
        postgresql-syntax = self.callHackageDirect {
          pkg = "postgresql-syntax";
          ver = "0.4.5.0";
          sha256 = "sha256-KY3hngBymjoBVSKfkNE5vQdwjoUXkJMcW6JUrcZcw+4=";
        } {};
      };
    };
  };
  nix-thunk = import ./nix-thunk {};
  ls = import ./listdirs.nix {
    inherit (nix-thunk) mapSubdirectories;
    inherit lib;
  };
  nix-filter = import ./nix-filter;
  filteredSource = nix-filter {
    root = ../.;
    include =
      ls ../. ["src" "examples" "integration-tests" "large-migration-test" "bench" "tests" "util" ]
      ++
      [
      (nix-filter.matchExt "hs")
      "./beam-automigrate.cabal"
      "./Setup.hs"
      "./cabal.project"
      "./README.md"
      "./README.lhs"
      "./LICENSE"
      ];
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "beam-automigrate" filteredSource {}) ghcs
