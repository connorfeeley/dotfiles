{ config
, lib
, pkgs
, ...
}:
{
  services.xserver.desktopManager.xfce.enable = true;
}
