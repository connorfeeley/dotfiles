{ lib, pkgs, ... }:

with lib; {
  # xsession = {
  #   enable = true;

  #   windowManager.command = "${pkgs.xmonad-config}/bin/xmonad";

  #   # HM module gets in the way; Launched with windowManager.command instead.
  #   windowManager.xmonad.enable = false;

  #   initExtra = ''
  #     # x11vnc -auth guess -forever -loop -noxdamage -repeat -rfbport 5900 -shared -safer -display :1
  #   '';
  # };

  home.packages = with pkgs; [
    xmonad-config

    dmenu
    haskellPackages.xmobar
    haskellPackages.yeganesh
    arandr
    libnotify
    xorg.xkill
    (polybar.override {
      pulseSupport = true;
      nlSupport = true;
    })

    redshift
    xorg.xbacklight

    x11vnc
  ];

  # For non-systemd plasma5 init:
  # xdg.configFile = {
  #   "plasma-workspace/env/set_window_manager.sh" = {
  #     executable = true;
  #     text = ''
  #       # export KDEWM=/home/cfeeley/source/xmonad-config/dist-newstyle/build/x86_64-linux/ghc-9.0.2/xmonad-config-0.1/x/xmonad/build/xmonad/xmonad
  #       export KDEWM=${pkgs.xmonad-config}/bin/xmonad
  #       exec ${pkgs.plasma-workspace}/bin/startplasma-x11
  #     '';
  #   };
  # };
}
