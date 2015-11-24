# Run with eg:
#
# NIX_PATH="nixpkgs=/path/to/my/nixpkgs" nix-shell
#
#
# Adapted from 'try-reflex/shell.nix'
#
{ system ? null }:

let nixpkgs               = import <nixpkgs> {};
    this                  = import ./try-reflex.nix nixpkgs;
    ourghc                = extendHaskellPackages this.ghc;
    platforms             = [ "ghc" ]; # [ "ghcjs" "ghc" ];
    extendHaskellPackages = haskellPackages: haskellPackages.override { overrides = self: super: {}; };
    reflexEnv             = platform: ourghc.ghcWithPackages
                                (p: import ./packages.nix { haskellPackages = p; inherit platform; });

in this.nixpkgs.runCommand "shell" {
  buildCommand = ''
    echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > $out
  '';
  buildInputs = [
    this.nixpkgs.nodejs
    this.nixpkgs.curl
    ourghc.cabal-install
    ourghc.ghcid

  ] ++ builtins.map reflexEnv platforms;
} ""
