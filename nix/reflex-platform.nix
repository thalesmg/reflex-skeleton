# $ nix-prefetch-url --unpack https://github.com/reflex-frp/reflex-platform/archive/develop.zip
let
  baseNixpkgs = import <nixpkgs> {};
  source = {
    reflex-platform = baseNixpkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "1527ba413d21caf07f568d9630170845da48dc6b";
      sha256 = "09mwhmdavs3pa5d30ahh1sfxfwpnggv9zaknv4ss37rp5jnf3006";
    };
  };
  reflex-platform = import source.reflex-platform { system = builtins.currentSystem; };
in
  reflex-platform
