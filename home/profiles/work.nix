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
  ] ++ lib.optionals isLinux (with pkgs; [
    remmina # RDP/VNC client
    freerdp

    # FIXME(darwin): broken
    pwrbar # control Kasa WiFi powerbar

    # FIXME(aarch64): eclipse dependency is unsupported on aarch64
    # dashboard
    zeuspack
  ]);
}
