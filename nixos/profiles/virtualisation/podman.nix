{ config, pkgs, lib, ... }:
let inherit (config.dotfield) guardian;
    docker-client = config.virtualisation.docker.package.override { clientOnly = true; };
in {
  environment.systemPackages = [
    # Do install the docker CLI to talk to podman.
    # Not needed when virtualisation.docker.enable = true;
    docker-client
  ];

  virtualisation.oci-containers.backend = "podman";
  virtualisation.containers.storage.settings.storage.driver =
    lib.mkIf (builtins.elem "zfs" config.boot.supportedFilesystems) "zfs";

  virtualisation.podman = {
    enable = true;
    enableNvidia = true;
    dockerCompat = !config.virtualisation.docker.enable;
    dockerSocket.enable = !config.virtualisation.docker.enable;
    defaultNetwork.settings.dns_enabled = true;

    # Allow remote access
    networkSocket.enable = false;
    networkSocket.server = "ghostunnel";
    networkSocket.tls.key = "/etc/ssl/server-key.pem";
    networkSocket.tls.cacert = "/etc/ssl/ca-cert.pem";
    networkSocket.tls.cert = "/etc/ssl/server-cert.pem";

    # ZFS storage backend
    extraPackages = [ pkgs.zfs ];

  };
  users.extraUsers.${guardian.username}.extraGroups = [ "podman" ];
}
