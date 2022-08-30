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

  services.dockerRegistry.storagePath = "/mnt/ssd/docker";

  users.users.${guardian.username}.extraGroups = ["docker"];
}
