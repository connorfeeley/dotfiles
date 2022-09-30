{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (config.lib) dotfield;
in {
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

  environment.darwinConfig = "${dotfield.fsPath}/lib/compat/darwin";

  # Administrative users on Darwin systems are part of the admin group by default.
  nix.settings.trusted-users = ["@admin" "@wheel"];

  environment.systemPackages = with pkgs; [
    coreutils

    #  Swiss Army Knife for macOS
    # => https://github.com/rgcr/m-cli
    m-cli
    mas
    terminal-notifier

    # A tool for managing macOS defaults.
    # https://github.com/malob/prefmanager
    # FIXME: `prefmanager` build fails with sandbox mode enabled
    # https://github.com/malob/prefmanager/issues/2
    # prefmanager
  ];

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  nix.configureBuildUsers = true;

  homebrew = {
    enable = true;
    onActivation.upgrade = true;
    onActivation.cleanup = "zap";
    global.lockfiles = true;

    masApps = {
      "Tailscale" = 1475387142;
      "Microsoft Remote Desktop" = 1295203466;
      "Xcode" = 497799835;
    };
    brews = [ ];
    casks = [
      "docker"

      "karabiner-elements"

      "google-chrome"
      "microsoft-edge"

      "microsoft-teams"
      "microsoft-outlook"

      "bitwarden"

      "nordvpn"
      "private-internet-access"

      "vlc"
      "xquartz"
      "lulu"
      "stats"
      "steam"
      "spotify"
      "deluge"
    ];
  };
  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
