let nix-thunk = import ./nix-thunk {};
in
with nix-thunk;
builtins.mapAttrs (_: x: import x {}) (mapSubdirectories thunkSource ./nixpkgs)
