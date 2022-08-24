{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    pwrbar   # control Kasa WiFi powerbar
    picocom  # minicom without the cruft
    remmina  # RDP/VNC client

    dtc      # device tree compiler
  ];
}
