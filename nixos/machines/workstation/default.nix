{ self, config, options, lib, pkgs, primaryUser, ... }:
let
  inherit (self.collective) peers;
  hostName = "workstation";

  inherit (config.lib.dotfiles.secrets) secretsDir secretsGroup;

  inherit (self.collective) hmArgs;
  inherit (config.lib.dotfiles.sys) notVm;

  hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";

in
{
  imports = [ ./hardware-configuration.nix ./zfs-root.nix ./mellanox.nix ./samba.nix ./tigervnc.nix ];

  # OKAY: make sure I don't bork my system remotely!
  # Bork bork: https://www.youtube.com/watch?v=i1H0leZhXcY
  assertions = lib.mkIf (!options.virtualisation ? qemu) [{
    # Ensure eth0 (motherboard ethernet) is using DHCP and that
    # tailscale, tailscaleUnlock, initrd networking, and initrd SSH are enabled.
    assertion = config.networking.interfaces.enp38s0.useDHCP
      && config.services.tailscale.enable
      && config.remote-machine.boot.tailscaleUnlock.enable
      && config.boot.initrd.network.enable
      && config.boot.initrd.network.ssh.enable;
    message = "Workstation may not be remotely accessible via tailscale.";
  }];

  # Mount /tmp as tmpfs
  boot.tmp.useTmpfs = true;
  boot.tmp.tmpfsSize = "75%"; # 75% of RAM

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  boot.crashDump.enable = true;

  # Trying to fix freezing on idle: https://wiki.archlinux.org/title/Ryzen#Soft_lock_freezing
  boot.kernelParams = [ "processor.max_cstate=5" "rcu_nocbs=0-23" ];

  system.stateVersion = "22.05";

  nix.settings = {
    system-features = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
  };

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
  environment.etc.timezone.source =
    "${pkgs.tzdata}/share/zoneinfo/${config.time.timeZone}";

  ### === networking ===========================================================

  networking =
    let
      host = peers.hosts.workstation;
      net = peers.networks.${host.network};
      interface = "enp38s0";
      hostName = "workstation";
    in
    {
      inherit hostName;

      hostId = "5679a857"; # Required for ZFS, but doesn't matter otherwise.

      useDHCP = false;
      usePredictableInterfaceNames = lib.mkDefault true;
      # interfaces.wlo1.useDHCP = true;

      firewall = {
        enable = false;
        allowedTCPPorts = [
          2049 # 2049: NFS
          5357 # wsdd

          # UxPlay (AirPlay)
          7000
          7001
          7100
        ];
        allowedUDPPorts = [
          3702 # wsdd

          # UxPlay (AirPlay)
          6000
          6001
          7011
        ];
      };

      defaultGateway = {
        inherit interface;
        inherit (net.ipv4) address;
      };

      # Motherboard 2.5GbE
      interfaces.${interface} = {
        useDHCP = true;
        ipv4.addresses = [{
          inherit (host.ipv4) address;
          inherit (net.ipv4) prefixLength;
        }];
        wakeOnLan.enable = true; # Enable Wake-on-LAN (default: magic packet)
      };

      # USB (lab) 1GbE ethernet
      interfaces.enp42s0f1u5u4u2 = {
        ipv4.addresses = [{
          address = "192.168.88.50";
          inherit (net.ipv4) prefixLength;
        }];
      };
    };

  boot.kernel.sysctl = {
    "net.ipv6.route.max_size" = 2147483647; # Default: 4096
    "net.ipv4.conf.default.rp_filter" = 2;
    "net.ipv4.conf.all.rp_filter" = 2;
  };

  boot.kernelModules = [ "usbip" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.usbip ];

  ### === Remote LUKS/ZFS Unlock  ============================================================

  # Enable tailscale in initrd
  remote-machine.boot.tailscaleUnlock = {
      enable = !options.virtualisation ? qemu;
      tailscaleStatePath = "/etc/secrets/initrd/tailscale-luks-setup.state";
    };

  # Enable networking and SSH server in initrd
  boot.initrd = lib.mkIf notVm {
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

  environment.systemPackages = with pkgs; [
    cryptsetup
    config.boot.kernelPackages.usbip
    # input-leap
    mstflint
    nixos-container
    procps
    fwupd
    openai-whisper
    linuxptp
    tigervnc
    virtiofsd
  ];

  programs.wireshark.enable = true;
  programs.wireshark.package = pkgs.wireshark;

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

  dotfiles.guardian = {
    enable = true;
    username = "cfeeley";
    autoLogin = false; # Enabling this dumped me to a TTY.
  };

  users.mutableUsers = false;
  users.users.root.hashedPassword = hashedPassword;
  users.users.cfeeley = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = hashedPassword;
    hashedPassword = hashedPassword;
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
      "sbuser"
    ] ++ (lib.optional config.networking.networkmanager.enable "networkmanager")
    ++ (lib.optional config.services.mysql.enable "mysql")
    ++ (lib.optional config.virtualisation.docker.enable "docker")
    ++ (lib.optional config.virtualisation.podman.enable "podman")
    ++ (lib.optional config.virtualisation.libvirtd.enable "libvirtd")
    ++ (lib.optional config.virtualisation.virtualbox.host.enable "vboxusers");
    shell = pkgs.zsh;
  };

  users.users.sbuser = {
    uid = 999;
    group = "sbuser";
    # isNormalUser = true;
    initialHashedPassword = hashedPassword;
    hashedPassword = hashedPassword;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups = [
      "users"
    ];
    shell = pkgs.bash;
  };
  users.groups.sbuser = {
    members = [ "sbuser" ];
    gid = 999;
  };

  home-manager.users = {
    "${config.dotfiles.guardian.username}" = {
      imports = with hmArgs.roles;
        (lib.flatten [
          hmArgs.profiles.core
          (_: { imports = [ ../../../lib/home ]; })
          hmArgs.modules
        ] ++ [
          self.inputs.nur.modules.homeManager.default

          self.inputs.nix-colors.homeManagerModules.default
          self.inputs.sops-nix.homeManagerModules.sops
          self.inputs.nix-colors.homeManagerModule
          self.inputs.nixos-vscode-server.nixosModules.home
          self.inputs.nix-index-database.hmModules.nix-index
        ]
        ### Roles
        ++ (with hmArgs.roles; workstation ++ personalised ++ developer ++ linux ++ emacs-config)
        ### Profiles
        ++ (with hmArgs.profiles; [
          core
          shells.fish
          desktop.vnc
          work
          nixos.chromium
          gstreamer
          media
          sync
          aws
          desktop.xmonad
          nixos.work
        ]));

      _module.args.inputs = self.inputs;

      home = {
        username = "cfeeley";
        homeDirectory = lib.mkForce "/home/cfeeley";
        stateVersion = "22.05";
      };
    };
  };

  programs.htop.enable = true;

  programs.atop = {
    enable = false;
    atopgpu.enable = true;
    netatop.enable = true;
    setuidWrapper.enable = true;
    atopService.enable = true;
    atopRotateTimer.enable = true;
  };

  programs.extra-container.enable = true;

  ###: --- module configuration ------------------------------------------------
  #: stylix
  # stylix.image = pkgs.fetchurl {
  #   # MacOS mojave wallpaper
  #   url = "https://512pixels.net/downloads/macos-wallpapers-thumbs/10-14-Day-Thumb.jpg";
  #   sha256 = "01r92v0062c8mbnhi2vya40l6mmhqwa25g23a6qnqzqq4iw78v0v";
  # };

  substituter.enable = true;

  # Use personal caches
  nix.caches = {
    enable = true;
    attic.enable = true;
    attic.upload.enable = true;
    cachix.enable = true;
  };

  # Enable attic Nix cache
  services.cache = {
    enable = true;
    enableCloudflareS3 = true;
    enablePostgres = true;
  };

  # Enable ZFS exporter
  services.prometheus.exporters.zfs = {
    enable = true;
    port = 9134;
  };

  services.x2goserver.enable = false;

  age.secrets = {
    dotfiles-readme-update-access-token = {
      file = "${secretsDir}/dotfiles-readme-update-access-token.txt.age";
      group = secretsGroup;
    };
  };

  services.vscode-server.enable = true;

  services.ntopng.enable = true;
  services.ntopng.httpPort = 9009;

  virtualisation.docker.daemon.settings.hosts =
    lib.mkIf config.virtualisation.docker.enable [
      "unix:///var/run/docker.sock"
      "tcp://0.0.0.0:2375"
    ];


  environment.shellAliases = {
    ip = "${pkgs.iproute} -c";
  };

  systemd.targets = {
    # Disable the GNOME3/GDM auto-suspend feature that cannot be disabled in GUI!
    # Normally the machine will power down after 20 minutes if no user is logged in.
    sleep.enable = false;
    sleep.unitConfig.DefaultDependencies = false;

    suspend.enable = false;
    suspend.unitConfig.DefaultDependencies = false;

    hibernate.enable = false;
    hibernate.unitConfig.DefaultDependencies = false;

    hybrid-sleep.enable = false;
    hybrid-sleep.unitConfig.DefaultDependencies = false;
  };
  powerManagement.enable = true;

  # FIXME(2023-02-28): always thinks VPN is disconnected
  # Poll VPN endpoint every 5 minutes and send an alert if the VPN is unreachable
  # systemd.user =
  #   let name = "vpn-connection-monitor";
  #   in {
  #     services.${name} =
  #       let
  #         notify =
  #           "${pkgs.libnotify}/bin/notify-send --urgency=critical --category=network --app-name=${name}";
  #         checkReachability = pkgs.writeShellScript "check-reachability" ''
  #           if ! ${pkgs.socat}/bin/socat -v - TCP:rossvideo.com:80,connect-timeout=10; then
  #             echo "Unreachable!"
  #             ${notify} "${name}: VPN unreachable"
  #           else
  #             echo "Reachable."
  #           fi
  #         '';
  #       in
  #       {
  #         description = "VPN status notification";

  #         wantedBy = [ "graphical-session.target" ];
  #         after = [ "network.target" "graphical-session.target" ];

  #         path = [ pkgs.socat pkgs.libnotify ];

  #         serviceConfig = {
  #           Type = "oneshot";
  #           ExecStart = checkReachability;
  #         };
  #       };
  #     timers.${name} = {
  #       wantedBy = [ "timers.target" ];
  #       timerConfig = {
  #         OnBootSec = "5m";
  #         OnUnitActiveSec = "5m";
  #         Unit = "vpn-connection-monitor.service";
  #       };
  #     };
  #   };

  # Jellyfin reverse proxy
  # Enable NGINX as a reverse proxy, with LetsEncrypt.
  services.nginx.enable = true;
  services.nginx.recommendedOptimisation = true;
  services.nginx.recommendedProxySettings = true;
  services.nginx.recommendedTlsSettings = true;
  services.nginx.recommendedGzipSettings = true;
  services.nginx.recommendedBrotliSettings = true;
  services.nginx.recommendedZstdSettings = true;
  # services.nginx.virtualHosts."workstation.elephant-vibes.ts.net" = {
  #   forceSSL = true;
  #   enableACME = true;
  #   # NOTE: path to certificate file - not the file itself, which we don't want added to the store
  #   sslCertificate = "/etc/secrets/tailscale/workstation.elephant-vibes.ts.net.crt";
  #   sslCertificateKey = "/etc/secrets/tailscale/workstation.elephant-vibes.ts.net.key";
  #   kTLS = true; # TLS in the kernel.
  #   # root = "/var/www/bikes.cfeeley.org";
  #   # locations."/foo/" = {
  #   #   proxyPass = "http://localhost:8082";
  #   #   proxyWebsockets = true; # needed if you need to use WebSocket
  #   #   extraConfig = "client_max_body_size 10G;" +
  #   #     # required when the target is also TLS server with multiple hosts
  #   #     "proxy_ssl_server_name on;" +
  #   #     # required when the server wants to use HTTP Authentication
  #   #     "proxy_pass_header Authorization;"
  #   #   ;
  #   # };
  # };
  security.acme = {
    acceptTerms = true;
    email = "admin@cfeeley.org";
  };
  # services.nginx = {
  #   enable = false;
  #   recommendedProxySettings = true;
  #   recommendedTlsSettings = true;
  #   # other Nginx options
  #   virtualHosts."${hostName}.${peers.networks.tailscale.domain}" = {
  #     forceSSL = true;
  #     # NOTE: path to certificate file - not the file itself, which we don't want added to the store
  #     sslCertificate =
  #       "/etc/secrets/tailscale/workstation.elephant-vibes.ts.net.crt";
  #     sslCertificateKey =
  #       "/etc/secrets/tailscale/workstation.elephant-vibes.ts.net.key";

  #     locations =
  #       let
  #         mkLocation = port: {
  #           proxyPass = "http://0.0.0.0:${toString port}/";
  #           proxyWebsockets = true; # needed if you need to use WebSocket
  #           extraConfig =
  #             # required when the target is also TLS server with multiple hosts
  #             "proxy_ssl_server_name on;" +
  #             # required when the server wants to use HTTP Authentication
  #             "proxy_pass_header Authorization;" +

  #         };
  #       in
  #       {
  #         # FIXME: services are not playing nice with reverse proxy
  #         "/qbittorrent/" = mkLocation 8080;
  #         "/jellyfin/" = mkLocation 9001;
  #         "/radarr/" = mkLocation 9002;
  #         "/sonarr/" = mkLocation 9003;
  #         "/jackett/" = mkLocation 9004;
  #         "/ntopng/" = mkLocation 9009;
  #         "/grafana/" = mkLocation 9010;
  #         "/prometheus/" = mkLocation 9011;
  #         "/rss/" = mkLocation 999;
  #       };
  #   };
  # };

  services.freshrss = {
    enable = false;
    passwordFile = "/etc/secrets/freshrss-password.txt";

    virtualHost = "${hostName}.${peers.networks.tailscale.domain}";
    baseUrl = "https://${hostName}.${peers.networks.tailscale.domain}/rss";
  };
  services.fwupd.enable = true;

  services.postgresql = rec {
    enable = true;
    package = pkgs.postgresql_16;
    extraPlugins = with package.pkgs; [ pg_partman pg_cron postgis pg_repack postgis timescaledb ];

    settings = {
      "cron.database_name" = "haskbike";
      shared_preload_libraries = "pg_cron,timescaledb";

      # Recommended settings for TimescaleDB (timescaledb-tune)
      shared_buffers = "8006MB";
      effective_cache_size = "24019MB";
      maintenance_work_mem = "2047MB";
      work_mem = "3416kB";
      "timescaledb.max_background_workers" = "16";
      max_worker_processes = "43";
      max_parallel_workers_per_gather = "12";
      max_parallel_workers = "24";
      wal_buffers = "16MB";
      min_wal_size = "512MB";
      max_wal_size = "1GB";
      default_statistics_target = "100";
      random_page_cost = "1.1";
      checkpoint_completion_target = "0.9";
      max_connections = "100";
      max_locks_per_transaction = "256";
      autovacuum_max_workers = "10";
      autovacuum_naptime = "10";
      effective_io_concurrency = "256";
    };

    # NOTE: comment out ensureDatabases and ensureUsers before upgrading version.
    ensureDatabases = [ "cfeeley" "haskbike" ];
    ensureUsers = [
      {
        name = "cfeeley";
        ensureDBOwnership = true;
        ensureClauses = {
          superuser = true;
          createrole = true;
          createdb = true;
        };
      }
      {
        name = "haskbike";
        ensureDBOwnership = true;
        ensureClauses = {
          superuser = true;
          createrole = true;
          createdb = true;
        };
      }
    ];

    enableTCPIP = true;
    authentication = lib.mkOverride 10 ''
      # type database DBuser auth-method
      local  all      all    trust

      # type database DBuser origin-address auth-method
      # IPv4
      host   all      all     127.0.0.1/32   trust
      # Tailscale
      host   all      all     100.0.0.0/8    trust
      # IPv6
      host all        all     ::1/128        trust

    '';
  };
}
