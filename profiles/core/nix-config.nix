{ lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isDarwin;

  substituters = [
    "https://cache.nixos.org/"
    "https://nix-community.cachix.org"
    "https://cfeeley.cachix.org"
    "https://nixpkgs-wayland.cachix.org"
    "https://cache.iog.io"
  ];
  trusted-substituters = substituters;

  nix-doc-path =
    if isDarwin then "${pkgs.nix-doc}/lib/libnix_doc_plugin.dylib"
    else "${pkgs.nix-doc}/lib/libnix_doc_plugin.so";
in
{
  environment.systemPackages = [ pkgs.nix-doc ];
  nix = {
    package = pkgs.nixUnstable;
    settings = {
      inherit substituters trusted-substituters;

      sandbox = lib.mkDefault (!pkgs.stdenv.hostPlatform.isDarwin);
      # FIXME: dangerous
      allowed-users = [ "*" ];
      trusted-users = [
        "root"
        "cfeeley"
        "nix-ssh" # user for store served with sshServe.enable
        "@wheel"
        "@admin" # Administrative users on Darwin systems are part of the admin group by default.
      ];

      # Prevent shells from being automatically garbage-collected
      keep-outputs = true;
      keep-derivations = true;

      # The  number  of  seconds a downloaded tarball is considered fresh.
      tarball-ttl = 3600 * 12;

      # Remote builders should fetch as many dependencies as possible,
      # rather than waiting for the current host to upload them.
      builders-use-substitutes = true; #: default: false

      # Number of tail lines to print when a build fails
      log-lines = 25; #: default: 10

      # Whether to use the flake evaluation cache
      eval-cache = true;

      # Ignore exceptions inside 'tryEval' calls when using --debugger
      ignore-try = true;

      # Automatically collect garbage if free space drops below 1GiB
      min-free = 1024 * 1024 * 1024; #: default: -1
      # Garbage collect up to 10GiB when min-free is triggered
      max-free = 1024 * 1024 * 1024 * 10;

      # Default commit message for 'nix flake update --commit-lock-file'
      commit-lockfile-summary = "chore(flake): update lock";

      # How often (in seconds) to poll for locks
      build-poll-interval = 1;

      # FIXME: broken
      # bash-prompt = "\[\e[00;34m\]λ \W \[\e[0m\]";

      # Interesting!
      # pre-build-hook = "";
      # post-build-hook = "";

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cfeeley.cachix.org-1:b+RrHsy/4WWys2o6T4YyF66OhdiZUF/R/N46JcS0HJU="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];

      # Load nix-doc pluign
      plugin-files = nix-doc-path; # Plugins to be loaded by nix
    };

    gc = {
      automatic = false;
    };

    extraOptions = ''
      warn-dirty = false
      allow-import-from-derivation = true
      experimental-features = repl-flake ca-derivations
    '';

    # FUP Options {{
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/166d6ebd9f0de03afc98060ac92cba9c71cfe550/lib/options.nix
    linkInputs = true;
    generateRegistryFromInputs = true;
    generateNixPathFromInputs = true;
    # }}
  };
}
