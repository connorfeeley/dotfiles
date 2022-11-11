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
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

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
      imports =
        (with hmArgs.roles; shell ++ remote) ++
        (with hmArgs.profiles; [ direnv ]);

      programs.termite.enable = false;
    };
  };

  ### === misc ================================================================

  system.stateVersion = "22.05";

  ### === rosetta ================================================================
  # https://xyno.space/post/nixos-utm-rosetta

  boot.initrd.availableKernelModules = [ "virtiofs" ];
  fileSystems."/run/rosetta" = {
    device = "rosetta";
    fsType = "virtiofs";
  };
  fileSystems."/media/share" = {
    device = "share";
    fsType = "virtiofs";
  };
  nix.settings.extra-platforms = [ "x86_64-linux" ];
  nix.settings.extra-sandbox-paths = [ "/run/rosetta" "/run/binfmt" ];
  boot.binfmt.registrations."rosetta" = {
    # based on https://developer.apple.com/documentation/virtualization/running_intel_binaries_in_linux_vms_with_rosetta#3978495
    interpreter = "/run/rosetta/rosetta";
    fixBinary = true;
    wrapInterpreterInShell = false;
    matchCredentials = true;
    magicOrExtension = ''\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x3e\x00'';
    mask = ''\xff\xff\xff\xff\xff\xfe\xfe\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'';
  };

  boot.cleanTmpDir = true;
  zramSwap.enable = true;
  networking.hostName = "rosy";
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBxw8UnnH5Cizu7p9r4PFGDe/azUrdC0qA3K9GtWtvf/+l4dy044X3mI+hHVigTbxDH5viYcTiH6Lk+SHl2uZuX6fkzTBaFoonEJrKeCRS25TTMmas9g7D/maDoENEF1X0acs5Ffk3CAqKlOeynGPnj4M1ovUM8wyg1lsfZXA+LVr9GLLziiZSxVBBjG341hfVP3LFijj8qIAoDnBPrlLBjrrCsHXZa1QxjjyQADC5Ty7wgqLZqhfEEmkSdUEdkEt1lW4wzJzNXM/7F+iBmLTTp2KcUTPP2kyCU8YR+QvOMafB7ufmRoMf2ERjQtCwSJCYfEot3DBOvdgL0lFBTW4T /Users/cfeeley/.ssh/id_rsa"
  ];

  services.openssh = lib.mkDefault {
    enable = true;
    openFirewall = true;
    # Authorized keys and permitRootLogin are set in ssh-host profile
  };

}
