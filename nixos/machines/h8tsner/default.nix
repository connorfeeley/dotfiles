# ##
### A hetzner VM for hosting a private modded minecraft server.
###
# Useful flakes:
# - https://github.com/Infinidoge/nix-minecraft
# - https://github.com/mkaito/nixos-modded-minecraft-servers
# FIXME: hardware config
{ config, lib, pkgs, primaryUser, modulesPath, ... }:
let
  inherit (config.dotfiles) guardian;

  # Bootstrap sets up:
  # - enabling systemd-boot
  # - a 'nixos' NixOS and HM user
  # https://github.com/nix-community/nixos-install-scripts/blob/master/hosters/hetzner-dedicated/hetzner-dedicated-wipe-and-install-nixos.sh
in
{
  ### === networking ================================================================

  # Via: https://nixos.wiki/wiki/Install_NixOS_on_Hetzner_Online
  # This make sure that our interface is named `eth0`.
  # This should be ok as long as you don't have multiple physical network cards
  # For multiple cards one could add a netdev unit to rename the interface based on the mac address
  # networking.usePredictableInterfaceNames = false;
  # systemd.network = {
  #   enable = true;
  #   networks."${interface}".extraConfig = ''
  #     [Match]
  #     Name = ${interface}
  #     [Network]
  #     # Add your own assigned ipv6 subnet here here!
  #     # Address = ${host.ipv6.address}/${host.ipv6.prefixLength}
  #     # Gateway = ${net.ipv6.address}
  #     # optionally you can do the same for ipv4 and disable DHCP (networking.dhcpcd.enable = false;)
  #     Address =  ${host.ipv4.address}/${host.ipv4.prefixLength}
  #     Gateway = ${net.ipv4.address}
  #   '';
  # };
  networking = {
    hostName = "h8tsner";
    domain = "";

    nameservers = [
      "8.8.8.8"
    ];
    defaultGateway = "172.31.1.1";
    defaultGateway6 = {
      address = "fe80::1";
      interface = "eth0";
    };
    dhcpcd.enable = true;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address = "5.161.248.248"; prefixLength = 32; }
        ];
        ipv6.addresses = [
          { address = "2a01:4ff:f0:dc18::1"; prefixLength = 64; }
          { address = "fe80::9400:3ff:fe84:b9d8"; prefixLength = 64; }
        ];
        ipv4.routes = [{ address = "172.31.1.1"; prefixLength = 32; }];
        ipv6.routes = [{ address = "fe80::1"; prefixLength = 128; }];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="96:00:03:84:b9:d8", NAME="eth0"

  '';

  ### === timezone ============================================================

  time = {
    timeZone = "America/Toronto";
    hardwareClockInLocalTime = true;
  };
  environment.sessionVariables.TZ = "${config.time.timeZone}";
  location = {
    provider = "manual";
    latitude = 43.70011;
    longitude = -79.4163;
  };

  ### === users ================================================================

  dotfiles.guardian.enable = true;
  dotfiles.guardian.username = "cfeeley";

  users.mutableUsers = false;
  users.users.root.hashedPassword =
    "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
  # Authorized keys and PermitRootLogin set in ssh-host
  users.users.cfeeley = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword =
      "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
    hashedPassword =
      "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups = [
      "wheel"
      "video"
      "audio"
      "networkmanager"
      "dialout"
      "cfeeley"
      "secrets"
      "wireshark"
    ] ++ (lib.optional config.services.mysql.enable "mysql")
    ++ (lib.optional config.virtualisation.docker.enable "docker")
    ++ (lib.optional config.virtualisation.podman.enable "podman");

    # Set user's shell
    shell = pkgs.fish;
  };

  home-manager.users = {
    cfeeley = hmArgs: {
      home.stateVersion = "23.11";
      programs.termite.enable = false;
    };
  };

  ### === misc ================================================================

  system.stateVersion = "23.11";

  services.openssh.enable = true;

  # Bluesky feed
  networking.firewall.allowedTCPPorts = [ 8000 443 3000 80 ];

  services.nginx.enable = true;
  services.nginx.virtualHosts."dev.cfeeley.org" = {
    addSSL = true;
    enableACME = true;
    serverAliases = [ "dev.cfeeley.org" ];
    root = "/var/www/dev.cfeeley.org";
  };
  security.acme = {
    acceptTerms = true;
    email = "admin@cfeeley.org";
  };


  ### === hardware ================================================================

  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "xen_blkfront" "vmw_pvscsi" ];
  boot.initrd.kernelModules = [ "nvme" ];
  fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4"; };

}
