# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: MIT

{ config, lib, pkgs, ... }:

let inherit (pkgs.stdenv) isLinux isDarwin isAarch64;
in {
  containers.jellyfin = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = "192.168.100.10";
    localAddress = "192.168.100.11";
    hostAddress6 = "fc00::1";
    localAddress6 = "fc00::2";

    config = { config, pkgs, ... }: {
      system.stateVersion = "22.11";

      networking.firewall = {
        enable = true;
        allowedTCPPorts = [ 8086 8920 ];
      };

      # Manually configure nameserver. Using resolved inside the container seems to fail currently
      environment.etc."resolv.conf".text = "nameserver 8.8.8.8";

      services.jellyfin.enable = true;
      # systemd.services.jellyfin = lib.mkIf config.services.jellyfin.enable {
      #   serviceConfig.PrivateUsers = lib.mkForce false;
      #   serviceConfig.PermissionsStartOnly = true;
      #   preStart = ''
      #     set -x
      #     #${pkgs.acl}/bin/setfacl -Rm u:jellyfin:rwX,m:rw-,g:jellyfin:rwX,d:u:jellyfin:rwX,d:g:jellyfin:rwX,o:---,d:o:---,d:m:rwx,m;rwx /home/dguibert/Videos/Series/ /home/dguibert/Videos/Movies/
      #     ${pkgs.acl}/bin/setfacl -m user:jellyfin:r-x /home/dguibert
      #     ${pkgs.acl}/bin/setfacl -m user:jellyfin:r-x /home/dguibert/Videos
      #     ${pkgs.acl}/bin/setfacl -m user:jellyfin:rwx /home/dguibert/Videos/Series
      #     ${pkgs.acl}/bin/setfacl -m user:jellyfin:rwx /home/dguibert/Videos/Movies
      #     ${pkgs.acl}/bin/setfacl -m group:jellyfin:r-x /home/dguibert
      #     ${pkgs.acl}/bin/setfacl -m group:jellyfin:r-x /home/dguibert/Videos
      #     ${pkgs.acl}/bin/setfacl -m group:jellyfin:rwx /home/dguibert/Videos/Series
      #     ${pkgs.acl}/bin/setfacl -m group:jellyfin:rwx /home/dguibert/Videos/Movies
      #     set +x
      #   '';
      #   unitConfig.RequiresMountsFor = "/home/dguibert/Videos";
      # };
      # systemd.tmpfiles.rules = [
      #   "L /var/lib/jellyfin/config - - - - /persist/var/lib/jellyfin/config"
      #   "L /var/lib/jellyfin/data   - - - - /persist/var/lib/jellyfin/data"
      # ];
      networking.firewall.interfaces."bond0".allowedTCPPorts = [
        8096 /*http*/
        8920 /*https*/
      ];
    };
  };
}
