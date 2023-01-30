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
    autoStart = true;
    checkClientCert = false;
    address = ":24800";
    screenName = "workstation";
    configFile = "/home/${config.dotfield.guardian.username}/.config/Debauchee/Barrier.conf";
  };
}
