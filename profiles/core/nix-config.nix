{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv) isDarwin;

  substituters = [
    "https://nix-community.cachix.org"
    "https://cfeeley.cachix.org"
  ];
  trusted-substituters = substituters;
in
{
  nix = {
    package = pkgs.nixVersions.latest;
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
      builders-use-substitutes = true; # : default: false

      # Number of tail lines to print when a build fails
      log-lines = 25; # : default: 10

      # Whether to use the flake evaluation cache
      eval-cache = true;

      # Ignore exceptions inside 'tryEval' calls when using --debugger
      ignore-try = true;

      # Automatically collect garbage if free space drops below 1GiB
      min-free = 1024 * 1024 * 1024; # : default: -1
      # Garbage collect up to 10GiB when min-free is triggered
      max-free = 1024 * 1024 * 1024 * 10;

      # Default commit message for 'nix flake update --commit-lock-file'
      commit-lockfile-summary = "chore(flake): update lock";

      # How often (in seconds) to poll for locks
      build-poll-interval = 1;

      warn-dirty = false;

      allow-import-from-derivation = true;

      # TODO: consider adding 'auto-allocate-uids discard-references'
      experimental-features =
        [ "nix-command" "flakes" "repl-flake" "ca-derivations" ];

      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cfeeley.cachix.org-1:b+RrHsy/4WWys2o6T4YyF66OhdiZUF/R/N46JcS0HJU="
      ];
    };

    gc = { automatic = false; };

    # FUP options.nix vendored at ../../modules/fup-options.nix
    linkInputs = true;
    generateRegistryFromInputs = true;
    generateNixPathFromInputs = true;
  };

  environment.systemPackages = with pkgs; [
    (writeShellApplication {
      name = "nix-stray-roots";
      runtimeInputs = [ config.nix.package pkgs.gnugrep ];
      text = ''
        nix-store --gc --print-roots | grep -E --color=auto -v "^(/nix/var|/run/\w+-system|\{memory)"
      '';
    })
  ];
}
