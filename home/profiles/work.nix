{ self', lib, pkgs, ... }:
let inherit (pkgs.stdenv.hostPlatform) isLinux;
in {
  home.packages = with pkgs;
    [
      picocom # minicom without the cruft
      dtc # device tree compiler

      self'.packages.pwrbar # control Kasa WiFi powerbar
      python3Packages.python-kasa
    ] ++ lib.optionals isLinux (with pkgs; [
      remmina # RDP/VNC client
      freerdp
      gparted # device tree compiler
    ]);
}
