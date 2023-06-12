# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, lib, pkgs, ... }:

let
  welcomeMessage = "visionfive-nix";
  xtermWelcomeScript = pkgs.writeScript "xterm-visionfive-welcome.sh" ''
    ${pkgs.figlet}/bin/figlet -c ${welcomeMessage}
    ${pkgs.neofetch}/bin/neofetch
    sh
  '';
in
{
  boot.supportedFilesystems = lib.mkForce [ "btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs" "ext4" "vfat" ];
  users = {
    users.default = {
      password = "visionfive-nix";
      isNormalUser = true;
      extraGroups = [
        "wheel"
      ];
    };
    users.root.hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
  };

  boot.kernelParams = [ "boot.shell_on_fail" ];

  # Enable ssh on boot
  services.openssh.enable = true;

  # Open port 19999 for Netdata
  networking.firewall.allowedTCPPorts = [ 19999 ];
  services.netdata.enable = false; # FIXME: broken

  # Enable Avahi mDNS, you should be able to reach http://visionfive-nix:19999
  # to reach netdata when booted
  services.avahi = {
    openFirewall = true;
    nssmdns = true; # Allows software to use Avahi to resolve.
    enable = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
    };
  };

  hardware.opengl.enable = true;
  services.cage = {
    enable = true;
    user = "default";
    program = "${pkgs.xterm}/bin/xterm -fa 'Monospace' -fs 14 -bg black -fg white -e sh -c '${xtermWelcomeScript}'";
  };
}
