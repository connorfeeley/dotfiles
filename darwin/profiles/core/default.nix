{ config
, lib
, pkgs
, ...
}:
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

  programs.bash = {
    enable = true;
    enableCompletion = true;
  };

  programs.zsh.enable = true;

  programs.tmux.enableSensible = true;

  environment.enableAllTerminfo = true;

  environment.systemPackages = with pkgs; [
    #  Swiss Army Knife for macOS
    # => https://github.com/rgcr/m-cli
    m-cli
    mas
    terminal-notifier
    darwin.trash
    darwin.lsusb # lsusb for MacOS

    prefmanager #   <- # A tool for managing macOS defaults.
    wifi-password # <- # what was that password again?
  ];

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  nix.configureBuildUsers = true;

  homebrew = {
    enable = true;
    onActivation = {
      upgrade = false;
      cleanup = "zap";
    };
    global = {
      lockfiles = true; # Generate lockfiles when running 'brew bundle' manually
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

  # Add homebrew packages to PATH
  environment.systemPath = [ config.homebrew.brewPrefix ];

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
