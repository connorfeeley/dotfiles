{ config, ... }:
let inherit (config.dotfield) guardian;
in {
  virtualisation.oci-containers.backend = "docker";

  virtualisation.docker = {
    enable = true;
    daemon.settings = {
      # Default: /var/lib/docker
      data-root = "/var/lib/docker"; # "/mnt/ssd/docker";
    };

    # FIXME: https://github.com/NixOS/nixpkgs/issues/158459
    storageDriver = "zfs";

    # Fixes nixos hanging on shutdown for a few minutes
    liveRestore = false;
  };

  users.users.${guardian.username}.extraGroups = [ "docker" ];
}
