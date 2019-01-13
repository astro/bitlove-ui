{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc862" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
