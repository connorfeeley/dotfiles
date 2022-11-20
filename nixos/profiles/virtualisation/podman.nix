{ config
, lib
, pkgs
, ...
}:
let
  inherit (config.dotfield) guardian;
in
{
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    defaultNetwork.dnsname.enable = true;
  };
  users.users.${guardian.username}.extraGroups = [ "podman" ];
}
