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
  inherit (config.lib.dotfield.secrets) mkAgeSecret;
in
{
  imports = [
    ./hardware-configuration.nix
  ];

  # OKAY: make sure I don't bork my system remotely!
  # Bork bork: https://www.youtube.com/watch?v=i1H0leZhXcY
  assertions = lib.mkIf (!config.nixos-vm.enable) [{
    # Ensure eth0 (motherboard ethernet) is using DHCP and that
    # tailscale, tailscaleUnlock, initrd networking, and initrd SSH are enabled.
    assertion =
      config.networking.interfaces.eth0.useDHCP &&
      config.services.tailscale.enable &&
      config.remote-machine.boot.tailscaleUnlock.enable &&
      config.boot.initrd.network.enable &&
      config.boot.initrd.network.ssh.enable;
    message = "Workstation may not be remotely accessible via tailscale.";
  }];

  # FIXME: does this interfere with rEFInd? if not this, then i blame Windows.
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.supportedFilesystems = [ "ext4" "btrfs" ];
  boot.supportedFilesystems = [ "ext4" "btrfs" ];

  # Mount /tmp as tmpfs
  boot.tmpOnTmpfs = true;

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  virtualisation.vmVariant = {
    virtualisation.graphics = false;
  };

  system.stateVersion = "22.05";

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
  environment.etc.timezone.source = "${pkgs.tzdata}/share/zoneinfo/${config.time.timeZone}";

  ### === networking ===========================================================

  networking = lib.mkIf (!config.nixos-vm.enable) (
    let
      host = peers.hosts.${hostName};
      net = peers.networks.${host.network};
      interface = "eth0";
    in
    {
      useDHCP = false;
      usePredictableInterfaceNames = false;
      # interfaces.wlp6s0.useDHCP = true;

      firewall = {
        enable = true;
        allowedTCPPorts = [ 2049 ]; # 2049: NFS
      };

      defaultGateway = {
        inherit interface;
        inherit (net.ipv4) address;
      };

      interfaces.${interface} = {
        useDHCP = true;
        ipv4.addresses = [{
          inherit (host.ipv4) address;
          inherit (net.ipv4) prefixLength;
        }];
      };
      interfaces.eth1 = {
        ipv4.addresses = [{
          address = "192.168.88.50";
          inherit (net.ipv4) prefixLength;
        }];
      };
    }
  );

  ### === Remote LUKS Unlock  ============================================================

  # Enable tailscale in initrd
  remote-machine.boot.tailscaleUnlock = {
    enable = true;
    tailscaleStatePath = "/etc/secrets/initrd/tailscale-luks-setup.state";
  };

  # Enable networking and SSH server in initrd
  boot.initrd = {
    # Driver for MSI (motherboard) 2.5GbE interface
    availableKernelModules = [ "r8169" ];

    network.enable = true;
    network.ssh = {
      enable = true;
      authorizedKeys = primaryUser.authorizedKeys;
      hostKeys = [
        # WARNING: DON'T USE AGE HERE
        "/etc/secrets/initrd/ssh_host_rsa_key"
        "/etc/secrets/initrd/ssh_host_ed25519_key"
      ];
    };
  };

  ### === Shares ============================================================
  fileSystems."/mnt/export/cfeeley" = {
    device = "/home/cfeeley";
    options = [ "bind" ];
  };

  services.nfs.server.enable = true;
  services.nfs.server.exports = ''
    /mnt/export         100.66.73.0/24(rw,fsid=0,no_subtree_check,all_squash,anonuid=0,anongid=100)
    /mnt/export/cfeeley 100.66.73.0/24(rw,nohide,insecure,no_subtree_check,all_squash,anonuid=0,anongid=100)
  '';

  # services.webdav = {
  #   enable = true;
  #   user = "cfeeley";
  #   settings = {
  #     # Only expose via tailscale
  #     address = peers.hosts.${hostName}.tailscale;
  #     port = 33464;
  #     auth = false;
  #     tls = false;
  #     prefix = "/";
  #     debug = true; #FIXME

  #     # Default user settings (will be merged)
  #     scope = ".";
  #     modify = true;
  #     rules = [ ];
  #   };
  # };

  ### === users ================================================================

  dotfield.guardian = {
    enable = true;
    username = "cfeeley";
    autoLogin = true;
  };

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
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
      ++ (lib.optional config.networking.networkmanager.enable "networkmanager")
      ++ (lib.optional config.services.mysql.enable "mysql")
      ++ (lib.optional config.virtualisation.docker.enable "docker")
      ++ (lib.optional config.virtualisation.podman.enable "podman")
      ++ (lib.optional config.virtualisation.libvirtd.enable "libvirtd")
      ++ (lib.optional config.virtualisation.virtualbox.host.enable "vboxusers")
    ;
    shell = pkgs.zsh;
  };

  home-manager.users = {
    cfeeley = hmArgs: {
      imports = with hmArgs.roles; workstation ++ linux ++ (with hmArgs.profiles; [
        sync
        work

        # Systemd scripts
        nixos.work
      ]);
    };
  };

  programs.htop.enable = true;

  services.grafana = {
    enable = true;
    settings.server = {
      # Listening address and TCP port
      http_addr = "0.0.0.0";
      http_port = 9010;
      # Grafana needs to know on which domain and URL it's running:
      domain = config.networking.hostName;
    };
  };

  ###: --- module configuration ------------------------------------------------
  #: stylix
  stylix.image = pkgs.fetchurl {
    # MacOS mojave wallpaper
    url = "https://512pixels.net/downloads/macos-wallpapers-thumbs/10-14-Day-Thumb.jpg";
    sha256 = "01r92v0062c8mbnhi2vya40l6mmhqwa25g23a6qnqzqq4iw78v0v";
  };
}
