{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux;
in
{
  services.pantheon = {
    apps.enable = true;
    contractor.enable = true;
  };

  programs.pantheon-tweaks.enable = true;
  services.xserver.desktopManager.pantheon.enable = true;
}
