{ pkgs ? import <nixpkgs> {}
, reflex-platform ? import ./nix/reflex-platform.nix
, compiler ? "ghcjs"
}:

reflex-platform.project (_: {
  withHoogle = false;
  useWarp = true;

  packages = {
    reflex-skeleton = ./.;
  };

  overrides = self: super:
    let
      inherit (pkgs.lib.lists) fold;
    in
      fold (broken-test-pkg: acc:
        acc // { ${broken-test-pkg} = pkgs.haskell.lib.dontCheck super.${broken-test-pkg}; }
      )
        {} [ "Glob"
             "hourglass"
             "unliftio"
             "x509"
             "x509-validation"
             "tls"
             "mono-traversable"
             "conduit"
             "yaml"
             "hpack"
           ];

  shells = {
    ghc   = ["reflex-skeleton"];
    ghcjs = ["reflex-skeleton"];
  };
})
