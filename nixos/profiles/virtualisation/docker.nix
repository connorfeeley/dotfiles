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
    enableNvidia = true;
    daemon.settings = {
      data-root = "/mnt/ssd/docker";
    };
  };

  users.users.${guardian.username}.extraGroups = ["docker"];
}
