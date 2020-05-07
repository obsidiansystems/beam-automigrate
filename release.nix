{ reflex-platform ? import ./dep/reflex-platform {}
, ...
}: 
let inherit (reflex-platform) hackGet;
    inherit (reflex-platform.nixpkgs.haskell.lib) dontCheck;
    beam = hackGet ./dep/beam;
    x = reflex-platform.ghc.override {
      overrides = self: super: {
        beam-core = self.callCabal2nix "beam-core" (beam + "/beam-core") {};
        beam-migrate = self.callCabal2nix "beam-migrate" (beam + "/beam-migrate") {};
        beam-postgres = dontCheck (self.callCabal2nix "beam-postgres" (beam + "/beam-postgres") {});
        postgresql-simple = dontCheck (self.callCabal2nix "postgresql-simple" (hackGet ./dep/postgresql-simple) {});
      };
    };
in x.callCabal2nix "beam-migrate-prototype" ./. {}
