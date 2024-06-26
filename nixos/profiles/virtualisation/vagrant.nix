{ config, lib, pkgs, ... }: {
  environment.systemPackages = with pkgs; [ vagrant ];

  # https://nixos.wiki/wiki/Vagrant#Using_NFS_mounts
  services.nfs.server.enable = true;
  networking.firewall.extraCommands = lib.optionalString config.virtualisation.virtualbox.host.enable ''
    ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
  '';
}
