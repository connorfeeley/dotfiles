{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfield) guardian;
in {
  virtualisation.docker = {
    enable = true;
  };
  users.users.${guardian.username}.extraGroups = ["docker"];
}
