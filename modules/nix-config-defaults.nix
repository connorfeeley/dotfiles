# https://jackson.dev/post/nix-reasonable-defaults/
{ config
, lib
, ...
}:
let
  always = lib.mkDefault {
    ### Almost always set
    # The timeout (in seconds) for establishing connections in the binary cache substituter.
    # It corresponds to curl’s –connect-timeout option. A value of 0 means no limit.
    connect-timeout = 5; # Default: 0 (no limit)

    # The number of lines of the tail of the log to show if a build fails.
    log-lines = 25; # Default: 10

    # When free disk space in /nix/store drops below min-free during a build,
    # Nix performs a garbage-collection until max-free bytes are available or there is no more garbage.
    # A value of 0 (the default) disables this feature.
    min-free = 128000000; #  Default: 0. Reasonable: 128 MB
    max-free = 1000000000; # Default: 0. Reasonable:   1 GB
  };
  recommended = lib.mkDefault {
    ### Set if understood
    # Enable "experimental" flakes support.
    experimental-features = "nix-command flakes"; # Default: nix-command. Reasonable: nix-command flakes

    # If set to true, Nix will fall back to building from source if a binary substitute fails.
    # This is equivalent to the –fallback flag. The default is false.
    # NOTE: when false, will cause the build to fail if ANY of the caches are unavailable.
    fallback = true; # Default: false

    # Whether to warn about dirty Git/Mercurial trees.
    warn-dirty = false; # Default: true

    # If set to true, Nix automatically detects files in the store that have identical contents,
    # and replaces them with hard links to a single copy. This saves disk space.
    # If set to false (the default), you can still run nix-store –optimise to get rid of duplicate files.
    auto-optimise-store = true;
  };

  developer = lib.mkDefault {
    ### Set for developers
    # If true, the garbage collector will keep the outputs of non-garbage derivations.
    # If false (default), outputs will be deleted unless they are GC roots themselves (or reachable from other roots).
    # In general, outputs must be registered as roots separately.
    # However, even if the output of a derivation is registered as a root,
    # the collector will still delete store paths that are used only at build time
    # (e.g., the C compiler, or source tarballs downloaded from the network).
    # To prevent it from doing so, set this option to true.
    keep-outputs = true; # Default: false
    keep-derivations = true; # Default: false
  };

  cfg = config.nix.reasonableDefaults;
in
{
  options.nix.reasonableDefaults = {
    always.enable = lib.mkEnableOption "Nix settings that are always recommended." // { default = true; };
    recommended.enable = lib.mkEnableOption "Nix settings that are strongly recommended." // { default = true; };
    developer.enable = lib.mkEnableOption "Nix settings that are always recommended." // { default = true; };
  };

  config = {
    nix.settings = lib.mkMerge [
      (lib.optionals cfg.always.enable always)
      (lib.optionals cfg.recommended.enable recommended)
      (lib.optionals cfg.developer.enable developer)
    ];
  };
}
