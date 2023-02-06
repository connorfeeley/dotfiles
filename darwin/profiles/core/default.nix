moduleArgs @ { config
, lib
, pkgs
, ...
}:
let
  inherit (config.networking) hostName;
in
{
  nix.nixPath = [
    # TODO: This entry should be added automatically via FUP's `nix.linkInputs`
    # and `nix.generateNixPathFromInputs` options, but currently that doesn't
    # work because nix-darwin doesn't export packages, which FUP expects.
    #
    # This entry should be removed once the upstream issues are fixed.
    #
    # https://github.com/LnL7/nix-darwin/issues/277
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/issues/107
    "darwin=/etc/nix/inputs/darwin"
  ];

  # HACK: MacOS doesn't have an /etc/hostname file
  environment.etc."hostname".text = "${hostName}";

  programs.bash = {
    enable = true;
    enableCompletion = true;
  };

  programs.zsh.enable = true;

  programs.tmux.enableSensible = true;

  programs.nix-index.enable = true;

  environment.enableAllTerminfo = true;

  environment.systemPackages =
    let apple_complete = (lib.lowPrio pkgs.apple_complete);
    in with pkgs; [
      #  Swiss Army Knife for macOS
      # => https://github.com/rgcr/m-cli
      m-cli
      mas
      terminal-notifier
      darwin.trash
      darwin.lsusb #       <- lsusb for MacOS

      prefmanager #        <- a tool for managing macOS defaults.
      wifi-password #      <- what was that password again?

      macfuse-stubs #      <- MacOS port of FUSE
      sshfs-fuse #         <- sshfs for MacOS
      darwin.iproute2mac # <- MacOS implementation of iproute2
      apple_complete #     <- bash completions for MacOS
      maclaunch #          <- Manage your macOS startup items.
    ];

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
  services.nix-daemon = {
    enable = true;
    enableSocketListener = false; # 'true' causes connection refused error
  };
  nix.configureBuildUsers = true;

  homebrew = {
    enable = true;
    onActivation = {
      upgrade = false;
      cleanup = "zap";
    };
    global = {
      brewfile = true; # Use generated Brewfile in the nix store
      autoUpdate = false; # Don't auto-update formulae when running brew manually
    };
    caskArgs = {
      require_sha = false; # Casks must have a checksum
      no_binaries = false; # Enable linking of helper executables
      no_quarantine = true; # Disable quarantining of downloads
    };

    masApps = {
      "Tailscale" = 1475387142;
    };
    brews = [ ];
  };

  # Add homebrew and macports packages to PATH
  environment.systemPath = [ config.homebrew.brewPrefix "/opt/local/bin" ];

  environment.pathsToLink = [
    "/Applications"
  ];

  documentation = {
    # NOTE: All darwin-compatible documentation options are set in 'profiles/core'.
  };

  # Enable info and man pages
  programs = {
    info.enable = true;
    # Include "man" outputs of all systemPackages
    man.enable = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
