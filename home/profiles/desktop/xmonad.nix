{ config, lib, pkgs, ... }:

with lib;
{
  xsession = {
    enable = true;

    windowManager.command = "${pkgs.xmonad-config}/bin/xmonad";

    # HM module gets in the way; Launched with windowManager.command instead.
    windowManager.xmonad.enable = false;

    initExtra = ''
      # x11vnc -auth guess -forever -loop -noxdamage -repeat -rfbport 5900 -shared -safer -display :1
    '';
  };

  home.packages = with pkgs; [
    # Provides xmonad binary - pkgs.xmobar should be overlayed as xmobar-config
    xmonad-config

    # Provides xmobar-top, xmobar-bottom, etc
    xmobar-config

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

  programs.xmobar = {
    # Explicitly disabled - HM module gets in the way of xmobar-config
    enable = false;
    package = xmobar-config;
  };

  xdg.configFile = {
    "plasma-workspace/env/set_window_manager.sh" = {
      executable = true;
      text = ''
        export KDEWM="${pkgs.xmonad-config}/bin/xmonad"
      '';
    };
  };
}
