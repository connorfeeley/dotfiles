###
### A hetzner VM for hosting a private modded minecraft server.
###
# Useful flakes:
# - https://github.com/Infinidoge/nix-minecraft
# - https://github.com/mkaito/nixos-modded-minecraft-servers
# FIXME: hardware config
{ config
, lib
, pkgs
, profiles
, suites
, inputs
, primaryUser
, collective
, modulesPath
, ...
}:
let
  inherit (collective) peers;
  inherit (config.networking) hostName;
  inherit (config.dotfield) guardian;

  inherit (lib)
    mkForce
    ;

  host = peers.hosts.${hostName};
  net = peers.networks.${host.network};
  interface = "eth0";

  # Bootstrap sets up:
  # - enabling systemd-boot
  # - a 'nixos' NixOS and HM user
  bootstrap-graphical = import ./bootstrap-graphical.nix;
in
# https://github.com/nix-community/nixos-install-scripts/blob/master/hosters/hetzner-dedicated/hetzner-dedicated-wipe-and-install-nixos.sh
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
  networking.dhcpcd.enable = true;

  # Ban baddies
  services.fail2ban.enable = true;

  # :)
  environments.hetzner.tarpit.enable = true;

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

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cfeeley";

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
  # Authorized keys and PermitRootLogin set in ssh-host
  users.users.cfeeley = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
    hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups =
      [
        "wheel"
        "video"
        "audio"
        "networkmanager"
        "dialout"
        "cfeeley"
        "secrets"
        "wireshark"
      ]
      ++ (lib.optional config.services.mysql.enable "mysql")
      ++ (lib.optional config.virtualisation.docker.enable "docker")
      ++ (lib.optional config.virtualisation.podman.enable "podman")
    ;

    # Set user's shell
    shell = pkgs.fish;
  };

  home-manager.users = {
    "${guardian.username}" = hmArgs: {
      imports = with hmArgs.roles;
        shell;

      programs.termite.enable = false;
    };
  };

  ### === misc ================================================================

  system.stateVersion = "22.05";

  ### === hardware ================================================================

  # imports = [
  #   (modulesPath + "/profiles/qemu-guest.nix")
  # ];

  # boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sd_mod" "sr_mod" ];
  # boot.initrd.kernelModules = [ ];
  # boot.kernelModules = [ ];
  # boot.extraModulePackages = [ ];

  # fileSystems."/" =
  #   {
  #     device = "/dev/disk/by-uuid/81817827-b655-4c17-8c18-d274ffa1a3b3";
  #     fsType = "ext4";
  #   };

  # swapDevices = [ ];

  # # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # # (the default) this is the recommended approach. When using systemd-networkd it's
  # # still possible to use this option, but it's recommended to use it in conjunction
  # # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  # networking.useDHCP = lib.mkDefault true;
  # # networking.interfaces.enp1s0.useDHCP = lib.mkDefault true;

  # hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
