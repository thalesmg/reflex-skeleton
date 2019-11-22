# $ nix-prefetch-url --unpack https://github.com/reflex-frp/reflex-platform/archive/develop.zip
let
  baseNixpkgs = import <nixpkgs> {};
  source = {
    reflex-platform = baseNixpkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "8f4b8973a06f78c7aaf1a222f8f8443cd934569f";
      sha256 = "167smg7dyvg5yf1wn9bx6yxvazlk0qk64rzgm2kfzn9mx873s0vp";
    };
  };
  reflex-platform = import source.reflex-platform { system = builtins.currentSystem; };
in
  reflex-platform
