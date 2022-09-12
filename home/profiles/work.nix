{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux system;
in {
  home.packages = with pkgs; [
    pwrbar   # control Kasa WiFi powerbar
    picocom  # minicom without the cruft

    dtc      # device tree compiler
  ] ++ lib.optionals isLinux (with pkgs; [
    remmina  # RDP/VNC client
    freerdp
  ]);
}
