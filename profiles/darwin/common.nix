{ config, lib, pkgs, inputs, ... }:

{
  nix.nixPath = [
    # FIXME: This entry should be added automatically via FUP's `nix.linkInputs`
    # and `nix.generateNixPathFromInputs` options, but currently that doesn't
    # work because nix-darwin doesn't export packages, which FUP expects.
    #
    # https://github.com/LnL7/nix-darwin/issues/277
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/issues/107
    "darwin=/etc/nix/inputs/darwin"
  ];

  environment.darwinConfig = "$DOTFIELD_DIR/lib/compat/darwin";

  # Administrative users on Darwin are part of this group, not the `wheel` group.
  nix.trustedUsers = [ "@admin" ];

  nix.useSandbox = true;

  environment.systemPackages = with pkgs; [
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
  users.nix.configureBuildUsers = true;

  homebrew = {
    # enable = true;
    enable = false;
    autoUpdate = true;
    global.noLock = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
