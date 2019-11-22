{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc"
}:

let
  project = import ./default.nix { inherit pkgs compiler; };
in
  project.shells.${compiler}
