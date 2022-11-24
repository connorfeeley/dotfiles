{ config
, lib
, pkgs
, ...
}:
let
  inherit (config.lib.dotfield.sys) hasWayland;

  firefoxPackage =
    if hasWayland
    then pkgs.firefox-wayland
    else pkgs.firefox;
in
{
  services.xserver = {
    enable = true;
    layout = "dvorak";
    # FIXME: propagate to GNOME settings
    xkbOptions = "caps:ctrl_modifier,ctrl:swap_lalt_lctl";
    exportConfiguration = true;

    displayManager.startx.enable = false;
  };

  services.getty.autologinUser = config.dotfield.guardian.username;

  services.earlyoom.enable = true;

  console.useXkbConfig = true;

  # TODO: might only be available on master?
  # programs._1password-gui.enable = true;
  # programs._1password.enable = true;

  xdg.portal.enable = true;

  programs.gnupg.agent.enableBrowserSocket = true;

  # Hide cursor upon keystroke.
  services.unclutter = {
    enable = true;
    keystroke = true;
  };

  environment.variables = {
    MOZ_ENABLE_WAYLAND = lib.optionalString hasWayland "1";
  };

  environment.systemPackages =
    ([
      firefoxPackage
    ])
    ++ (lib.optionals hasWayland (with pkgs; [
      wl-clipboard

      # Grab images from a Wayland compositor
      # https://sr.ht/~emersion/grim/
      grim

      # Select a region in a Wayland compositor and print it to the standard output.
      # A complement to grim
      # https://github.com/emersion/slurp
      slurp
    ]));
}
