{ self', lib, pkgs, ... }:
let inherit (pkgs.stdenv.hostPlatform) isLinux;
in {
  home.packages = [
    pkgs.picocom # minicom without the cruft
    pkgs.dtc # device tree compiler

    self'.packages.pwrbar # control Kasa WiFi powerbar
    pkgs.python3Packages.python-kasa
  ] ++ lib.optionals isLinux [
    pkgs.remmina  # FIXME(2024-05-30): HM module not available on 23.11. Waiting for 24.05.
    pkgs.freerdp
    pkgs.gparted
  ];

  # RDP/VNC client
  # FIXME(2024-05-30): HM module not available on 23.11. Waiting for 24.05.
  # services.remmina = lib.mkIf isLinux {
  #   enable = true;
  #   addRdpMimeTypeAssoc = true;
  #   systemdService.enable = true;
  # };
}
