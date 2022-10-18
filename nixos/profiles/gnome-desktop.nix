{
  config,
  lib,
  pkgs,
  profiles,
  ...
}: {
  services.xserver.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Required for Firefox integration in home-manager
  services.gnome.gnome-browser-connector.enable = true;

  services.gnome.sushi.enable = true;
  # programs.gnupg.agent.pinentryFlavor = "gnome3";
}
