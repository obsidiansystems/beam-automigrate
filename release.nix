{ reflex-platform ? import ./dep/reflex-platform {}
, ...
}: 
let inherit (reflex-platform) hackGet;
    inherit (reflex-platform.nixpkgs.haskell.lib) dontCheck;
    beam = hackGet ./dep/beam;
    x = reflex-platform.ghc.override {
      overrides = self: super: {
        beam-core = self.callHackage "beam-core" "0.9.0.0" {};
        beam-migrate = self.callHackage "beam-migrate" "0.5.0.0" {};
        beam-postgres = dontCheck (self.callHackage "beam-postgres" "0.5.0.0" {});
      };
    };
in x.callCabal2nix "beam-automigrate" ./. {}
