{ config, lib, pkgs, ... }:
let
  inherit (config.lib.dotfield) fsPath;
  inherit (config.networking) hostName;

  darwinConfigPath = "${fsPath}/lib/compat/darwin";
in
{
  nix = {
    settings = {
      auto-optimise-store = true;
      # TODO: is it really reasonable to set these all as defaults?
      system-features = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    };

    nixPath = [ "nixos-config=${darwinConfigPath}" ];
  };

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
    let apple_complete = (lib.lowPrio pkgs.darwinPackages.apple_complete);
    in with pkgs; [
      #  Swiss Army Knife for macOS
      # => https://github.com/rgcr/m-cli
      m-cli
      mas
      terminal-notifier
      darwin.trash
      darwin.lsusb # <- lsusb for MacOS
      lsof

      prefmanager # <- a tool for managing macOS defaults.
      wifi-password # <- what was that password again?

      macfuse-stubs # <- MacOS port of FUSE
      sshfs-fuse # <- sshfs for MacOS
      darwin.iproute2mac # <- MacOS implementation of iproute2
      apple_complete # <- bash completions for MacOS
      darwinPackages.maclaunch # <- Manage your macOS startup items.
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
      autoUpdate =
        false; # Don't auto-update formulae when running brew manually
    };
    caskArgs = {
      require_sha = false; # Casks must have a checksum
      no_binaries = false; # Enable linking of helper executables
      no_quarantine = true; # Disable quarantining of downloads
    };

    masApps = { "Tailscale" = 1475387142; };
    brews = [ ];
  };

  # Add homebrew and macports packages to PATH
  environment.systemPath = [ config.homebrew.brewPrefix "/opt/local/bin" ];

  environment.pathsToLink = [ "/Applications" ];

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
