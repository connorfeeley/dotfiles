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
    defaultSession = "plasma+xmonad+xmonad";
    session = [
      {
        manage = "desktop";
        name = "plasma+xmonad";
        # start = ''exec env KDEWM=${pkgs.xmonad-config}/bin/xmonad ${pkgs.plasma-workspace}/bin/startplasma-x11'';
        start = ''exec env KDEWM=/media/psf/Home/source/xmonad-config/dist-newstyle/build/aarch64-linux/ghc-9.0.2/xmonad-config-0.1/x/xmonad/build/xmonad/xmonad ${pkgs.plasma-workspace}/bin/startplasma-x11'';
      }
    ];
  };

  qt5.platformTheme = "gnome";
}
