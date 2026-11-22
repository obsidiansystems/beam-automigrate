let nix-thunk = import ./nix-thunk {};
in
{
  unstable = import (nix-thunk.thunkSource ./nixpkgs/unstable) {};
}
