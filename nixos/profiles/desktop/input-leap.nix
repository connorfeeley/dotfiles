{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux;
in
{
  services.input-leap.server = {
    enable = true;
    configFile = "/home/${config.dotfield.guardian.username}/.config/Debauchee/Barrier.conf";
  };
}
