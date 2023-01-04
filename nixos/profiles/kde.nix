{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux;
in
{
  ###
  ### KDE
  ###
  services.xserver.desktopManager.plasma5 = {
    enable = true;
    supportDDC = true;
    useQtScaling = true;
    runUsingSystemd = true;
  };

  services.xserver.displayManager = {
    defaultSession = "plasma5+xmonad";
    session = [
      {
        manage = "desktop";
        name = "plasma5+xmonad";
        start = ''exec env KDEWM=${pkgs.xmonad-config}/bin/xmonad ${pkgs.plasma-workspace}/bin/startplasma-x11'';
      }
    ];
  };

  qt5.platformTheme = "gnome";
}
