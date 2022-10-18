{
  config,
  lib,
  pkgs,
  ...
}: let
  substituters = [
    "https://cache.nixos.org/"
    "https://dotfield.cachix.org"
    "https://nix-community.cachix.org"
    "https://nixpkgs-wayland.cachix.org"
    "https://cache.iog.io"
  ];
  trusted-substituters = substituters;
in {
  nix = {
    package = pkgs.nix;
    settings = {
      inherit substituters trusted-substituters;

      sandbox = lib.mkDefault (!pkgs.stdenv.hostPlatform.isDarwin);
      # FIXME: dangerous
      allowed-users = ["*"];
      trusted-users = ["root" "cfeeley"];

      # Remote builders should fetch as many dependencies as possible,
      # rather than waiting for the current host to upload them.
      builders-use-substitutes = true; #: default: false

      # Number of tail lines to print when a build fails
      log-lines = 25; #: default: 10


      # Automatically collect garbage if free space drops below 1GiB
      min-free = 1024 * 1024 * 1024; #: default: -1
      # Garbage collect up to 10GiB when min-free is triggered
      max-free = 1024 * 1024 * 1024 * 10;

      # Interesting!
      # pre-build-hook = "";
      # post-build-hook = "";

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
    };

    gc = {
      automatic = false;
    };

    extraOptions = ''
      warn-dirty = false
      allow-import-from-derivation = true
      experimental-features = repl-flake
    '';

    # FUP Options {{
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/166d6ebd9f0de03afc98060ac92cba9c71cfe550/lib/options.nix
    linkInputs = true;
    generateRegistryFromInputs = true;
    generateNixPathFromInputs = true;
    # }}
  };
}
