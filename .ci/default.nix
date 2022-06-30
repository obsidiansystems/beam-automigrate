{ }:
let
  nixpkgsSets = import ./nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck markUnbroken overrideCabal;
  beam-overrides = self: super: (import ./gargoyle { haskellPackages = self; }) // {
    beam-core = self.callHackageDirect {
      pkg = "beam-core";
      ver = "0.9.2.1";
      sha256 = "11hpjm9ywyd170ff2cm4zpfpvcan00df48c7v6yi4yylsps99apz";
    } {};
    beam-migrate = self.callHackageDirect {
      pkg = "beam-migrate";
      ver = "0.5.1.2";
      sha256 = "1wdga230465f01pvg0hlls1vk29lb6n3qs3cfax4mf5g63lgyjxw";
    } {};
    beam-postgres = dontCheck (self.callHackageDirect {
      pkg = "beam-postgres";
      ver = "0.5.2.1";
      sha256 = "1v1pkvk1swlpqnnzzs36h3v73g5bw7737z32zlybdgdjs9bw0yqy";
    } {});
    which = doJailbreak (self.callHackageDirect {
      pkg = "which";
      ver = "0.2";
      sha256 = "1g795yq36n7c6ycs7c0799c3cw78ad0cya6lj4x08m0xnfx98znn";
    } {});
    postgresql-syntax = dontCheck (self.callHackageDirect {
      pkg = "postgresql-syntax";
      ver = "0.4.1";
      sha256 = "0nwqiq7ffxmmklrcjm2szz5kn9yfdm57i47sq5wny86gnn3z54sz";
    } {});
  };
  ghcs = rec {
    ghc865 = nixos2003.haskell.packages.ghc865.override {
      overrides = self: super: beam-overrides self super //
      {
        headed-megaparsec = self.callHackageDirect {
          pkg = "headed-megaparsec";
          ver = "0.2.0.1";
          sha256 = "0p01cgpad8j9lylkzhhvrgcgvxjnbkfsab6yc6rdc40213289hab";
        } {};
        megaparsec = self.callHackageDirect {
          pkg = "megaparsec";
          ver = "9.0.1";
          sha256 = "1279c0snq1w13scikiakdm25ybpnvbpm7khjq4wyy0gj1vvh8r6z";
        } {};
        selective = self.callHackageDirect {
          pkg = "selective";
          ver = "0.4.2";
          sha256 = "12ca7npap8p8y9yrmxzsrq9zcmj713wf41g3rnkdymcwl1c20375";
        } {};
        postgresql-syntax = dontCheck (self.callHackageDirect {
          pkg = "postgresql-syntax";
          ver = "0.4";
          sha256 = "0p2xg0hnpcn8y2vgqpxlykzkrq19wx478pigi141l5ancglw9x8w";
        } {});
      };
    };
    ghc884 = nixos2003.haskell.packages.ghc884.override {
      overrides = beam-overrides;
    };
    ghc8107 = unstable.haskell.packages.ghc8107.override {
      overrides = beam-overrides;
    };
    ghc902 = unstable.haskell.packages.ghc902.override {
      overrides = self: super: {
        gargoyle = doJailbreak (markUnbroken super.gargoyle);
        gargoyle-postgresql = doJailbreak super.gargoyle-postgresql;
        gargoyle-postgresql-connect = doJailbreak super.gargoyle-postgresql-connect;
        gargoyle-postgresql-nix = overrideCabal
          (doJailbreak super.gargoyle-postgresql-nix)
          (drv: {
            librarySystemDepends = (drv.librarySystemDepends or []) ++ [
              unstable.postgresql
            ];
          });
      };
    };
  };
  nix-filter = import ./nix-filter;
  filteredSource = nix-filter {
    root = ../.;
    # TODO use mapSubdirectories
    include = [
      "src/Database/Beam"
      "src/Database/Beam/AutoMigrate"
      "src/Database/Beam/AutoMigrate/Schema"
      "examples"
      "integration-tests"
      "integration-tests/PostgresqlSyntax/Data"
      "large-migration-test"
      "bench"
      "tests"
      "tests/Test/Database/Beam/AutoMigrate"
      "util/Database/Beam/AutoMigrate"
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
