# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ ... }:

{
  # Enable DHCP server on 100G interfaces
  services.dhcpd4 = {
    enable = true;
    interfaces = [ "enp36s0f1np1" "enp36s0f0np0" ];
    extraConfig = ''
      option subnet-mask 255.255.255.0;

      subnet 192.168.20.0 netmask 255.255.255.0 {
        option broadcast-address 192.168.20.255;
        option routers 192.168.20.50;
        interface enp36s0f1np1;
        range 192.168.20.50 192.168.20.63;
      }
      subnet 192.168.21.0 netmask 255.255.255.0 {
        option broadcast-address 192.168.21.255;
        option routers 192.168.21.50;
        interface enp36s0f0np0;
        range 192.168.21.50 192.168.21.63;
      }
    '';
  };
}
