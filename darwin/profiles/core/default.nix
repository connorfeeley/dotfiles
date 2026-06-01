{ config, lib, pkgs, ... }:
let
  inherit (config.networking) hostName;
in
{
  nix = {
    settings = {
      auto-optimise-store = false; # Causes /nix/store/.links errors
      system-features = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    };

    daemonProcessType = "Interactive";
    daemonIOLowPriority = false;
  };

  # HACK: MacOS doesn't have an /etc/hostname file
  environment.etc."hostname".text = "${hostName}";

  programs.bash = {
    enable = true;
    completion.enable = true;
  };

  programs.zsh.enable = true;

  programs.tmux.enableSensible = true;

  programs.nix-index.enable = true;

  environment.enableAllTerminfo = false; # FIXME: switch back to 'true' once https://github.com/NixOS/nixpkgs/pull/522784 lands on a stable branch

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

      # prefmanager # <- a tool for managing macOS defaults.

      macfuse-stubs # <- MacOS port of FUSE
      sshfs-fuse # <- sshfs for MacOS
      darwin.iproute2mac # <- MacOS implementation of iproute2
      apple_complete # <- bash completions for MacOS
      darwinPackages.maclaunch # <- Manage your macOS startup items.
      unixtools.net-tools
    ];

  # Recreate /run/current-system symlink after boot
  services.nix-daemon = {
    enableSocketListener = false; # 'true' causes connection refused error
  };

  homebrew = {
    enable = true;
    onActivation = {
      upgrade = false;
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
  environment.systemPath = [ config.homebrew.prefix "/opt/local/bin" ];

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
