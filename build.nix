# Run with eg:
#
# NIX_PATH="nixpkgs=/path/to/my/nixpkgs" nix-build build.nix
#
let
  nixpkgs    = import <nixpkgs> {};
  reflexPkgs = import ./try-reflex.nix nixpkgs;
in
reflexPkgs.ghcjs.callPackage ./. {
    }

