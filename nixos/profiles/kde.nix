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
}
