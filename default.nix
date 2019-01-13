{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc862" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./bitlove-ui.nix { }
