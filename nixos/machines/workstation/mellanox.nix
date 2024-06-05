# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ inputs, config, pkgs, ... }:
let mft = config.programs.mellanoxFirmwareTools.package;
in {
  imports = [ inputs.nurpkgs.nixosModules.mellanox ];

  # Enable the Mellanox firmware tools
  programs.mellanoxFirmwareTools.enable = true;

  networking = {
    interfaces = {
      # Mellanox 100GbE port 0
      enp36s0f0np0.useDHCP = false;
      enp36s0f0np0.ipv4 = {
        addresses = [{
          address = "192.168.20.50";
          prefixLength = 24;
        }];

        # Add a static route for multicast traffic
        routes = [{
          address = "239.0.0.0";
          prefixLength = 8;
          type = "multicast";
        }];
      };

      # Mellanox 100GbE port 1
      enp36s0f0np1.useDHCP = false;
      enp36s0f0np1.ipv4 = {
        addresses = [{
          address = "192.168.21.50";
          prefixLength = 24;
        }];
      };
    };

    # Set up a dispatcher script to configure the Mellanox ports.
    networkmanager.dispatcherScripts = [{
      type = "pre-up";
      source = pkgs.writeShellScript "mellanoxUpHook" ''
        if [ "$2" != "up" ]; then
          logger "$0 exit: event $2 != up"
          exit
        fi

        # coreutils and iproute are in PATH too
        logger "Device $DEVICE_IFACE pre-up"
        case "$DEVICE_IFACE" in
          enp36s0f0np0)
            logger "$DEVICE_IFACE: setting FEC to RS, speed to 100G"
            ${mft}/bin/mst start
            ${mft}/bin/mlxlink -d /dev/mst/mt4121_pciconf0   --fec RS --fec_speed 100G
            ${pkgs.ethtool}/bin/ethtool -s ''${DEVICE_IFACE} speed 100000 duplex full autoneg off
            ;;
          enp36s0f0np1)
            logger "$DEVICE_IFACE: setting FEC to RS, speed to 100G"
            ${mft}/bin/mst start
            ${mft}/bin/mlxlink -d /dev/mst/mt4121_pciconf0.1 --fec RS --fec_speed 100G
            ${pkgs.ethtool}/bin/ethtool -s ''${DEVICE_IFACE} speed 100000 duplex full autoneg off
            ;;
          *)
            logger "$DEVICE_IFACE: not a Mellanox port"
            exit
            ;;
        esac
      '';
    }];
  };
}
