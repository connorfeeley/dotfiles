{ config
, lib
, pkgs
, ...
}:
let
  inherit (config.dotfield) guardian;
in
{
  virtualisation.docker = {
    enable = true;
    daemon.settings = {
      data-root = "/mnt/ssd/docker";
    };

    # Fixes nixos hanging on shutdown for a few minutes
    liveRestore = false;
  };

  users.users.${guardian.username}.extraGroups = [ "docker" ];
}
