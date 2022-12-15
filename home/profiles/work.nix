{ lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in
{
  home.packages = with pkgs; [
    picocom # minicom without the cruft
    dtc # device tree compiler

    pwrbar # control Kasa WiFi powerbar
  ] ++ lib.optionals isLinux (with pkgs; [
    remmina # RDP/VNC client
    freerdp
  ]);
}
