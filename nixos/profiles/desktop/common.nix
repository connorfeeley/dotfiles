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
  services.getty.autologinUser = config.dotfield.guardian.username;

  services.earlyoom.enable = true;

  console.useXkbConfig = true;

  programs.gnupg.agent.enableBrowserSocket = true;

  # Hide cursor upon keystroke.
  # services.unclutter = {
  #   enable = true;
  #   keystroke = true;
  # };

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
