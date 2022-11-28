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
    enable = false;
    daemon.settings = {
      # Default: /var/lib/docker
      data-root = "/var/lib/docker"; # "/mnt/ssd/docker";
    };

    # Fixes nixos hanging on shutdown for a few minutes
    liveRestore = false;
  };

  users.users.${guardian.username}.extraGroups = [ "docker" ];
}
