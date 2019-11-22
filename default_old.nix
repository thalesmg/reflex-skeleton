{ pkgs ? import <nixpkgs> {}
, reflex-platform ? import ./nix/reflex-platform.nix
, compiler ? "ghcjs"
}:

with pkgs;

let
  reflexHaskellPackages = reflex-platform.${compiler}.override {
    overrides = oldHaskellPackages: newHaskellPackages:
      {
        Glob = haskell.lib.dontCheck newHaskellPackages.Glob;
        hourglass = haskell.lib.dontCheck newHaskellPackages.hourglass;
      };
  };

  hsTools = with reflexHaskellPackages; [
    cabal2nix
    hpack
  ];

  drv = reflexHaskellPackages.callCabal2nix "reflex-skeleton" ./. {};

  dev = drv.env.overrideAttrs (attrs: {
    buildInputs = attr.buildInputs
                  ++ hsTools;
  });
in
  if lib.inNixShell then dev else drv
