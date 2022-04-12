{ }:
let
  ghcs = import ./ghc-beam-overrides.nix;
in
  lib.mapAttrs (_: ghc: ghc.overrides.callCabal2nix "beam-automigrate" ../. {}) ghcs
