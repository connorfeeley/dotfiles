{ config
, pkgs
, lib
, ...
}:
let
  inherit (config.dotfield) guardian;
in
{
  environment.systemPackages = [
    pkgs.arion

    # Do install the docker CLI to talk to podman.
    # Not needed when virtualisation.docker.enable = true;
    pkgs.docker-client
  ];

  virtualisation.oci-containers.backend = "podman";
  virtualisation.containers.storage.settings.storage.driver = lib.mkIf (builtins.elem "zfs" config.boot.supportedFilesystems) "zfs";

  virtualisation.podman = {
    enable = true;
    enableNvidia = true;
    dockerCompat = true;
    dockerSocket.enable = true;
    defaultNetwork.dnsname.enable = true;

    # Allow remote access
    networkSocket.enable = false;
    networkSocket.server = "ghostunnel";
    networkSocket.tls.key = "/etc/ssl/server-key.pem";
    networkSocket.tls.cacert = "/etc/ssl/ca-cert.pem";
    networkSocket.tls.cert = "/etc/ssl/server-cert.pem";


    # ZFS storage backend
    extraPackages = [ pkgs.zfs ];

  };
  users.extraUsers.${guardian.username}.extraGroups = ["podman"];
}
