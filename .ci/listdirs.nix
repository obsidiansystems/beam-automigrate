# Given a parent directory and a list of immediate child directories,
# this function will produce a list of all directory paths starting
# from each specified child directory.
#
# For example:
# > ls ./project [ "src" "tests" ]
# > [ "src" "src/Data" "src/Data/Map" "tests" "tests/Arbitrary" ]
#
{ mapSubdirectories # This function is defined in nix-thunk
, lib # from nixpkgs
}:
let
  f = root: tree: lib.flatten
    (lib.attrValues
      (lib.mapAttrs
        (k: v:
          let newroot = if root == "" then k else root + "/" + k;
          in [newroot] ++ f newroot v) tree));
  ls = mapSubdirectories (x: ls x);
  subdirs = parent: children: lib.filterAttrs (n: _: builtins.elem n children) (ls parent);
in parent: dirs: f "" (subdirs parent dirs)
