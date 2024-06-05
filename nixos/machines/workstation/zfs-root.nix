{ config, options, lib, pkgs, ... }:
# Don't configure ZFS for VMs
lib.mkIf (!options.virtualisation ? qemu) {
  boot.loader.efi.efiSysMountPoint = "/boot";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.generationsDir.copyKernels = true;
  # boot.loader.grub.enable = true;
  # boot.loader.grub.copyKernels = true;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.gfxmodeEfi = "2560x1440";
  # boot.loader.grub.zfsSupport = true;
  # boot.loader.grub.extraPrepareConfig = ''
  #   mkdir -p /boot/efis
  #   for i in  /boot/efis/*; do mount $i ; done

  #   mkdir -p /boot/efi
  #   mount /boot/efi
  # '';
  # boot.loader.grub.extraInstallCommands = ''
  #   ESP_MIRROR=$(${pkgs.coreutils}/bin/mktemp -d)
  #   ${pkgs.coreutils}/bin/cp -r /boot/efi/EFI $ESP_MIRROR
  #   for i in /boot/efis/*; do
  #    ${pkgs.coreutils}/bin/cp -r $ESP_MIRROR/EFI $i
  #   done
  #   ${pkgs.coreutils}/bin/rm -rf $ESP_MIRROR
  # '';
  # boot.loader.grub.devices =
  #   [ "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S59ANMFNB30863T" ];

  boot.kernelParams = [
    # https://github.com/openzfs/zfs/issues/10255#issuecomment-1067415974
    # Max ARC (Adaptive Replacement Cache) size: 18GB
    "zfs.zfs_arc_max=${toString (4 * 1024 * 1024 * 1024)}" # 4 GB

    # Target number of bytes the ARC should leave as free memory on the system
    "zfs.zfs_arc_sys_free=${toString (24 * 1024 * 1024 * 1024)}" # 24 GiB
  ];

  boot.zfs.extraPools = lib.optionals (!config.nixos-vm.enable) [ "rpool" ];

  fileSystems."/" = {
    device = "npool/nixos/root";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/home" = {
    device = "npool/nixos/home";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/var/lib" = {
    device = "npool/nixos/var/lib";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/var/log" = {
    device = "npool/nixos/var/log";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  # fileSystems."/boot" = {
  #   device = "bpool/nixos/root";
  #   fsType = "zfs";
  #   options = [ "zfsutil" "X-mount.mkdir" ];
  # };

  # EFI partion
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/E342-9852"; # "-part1"
    fsType = "vfat";
    options = [ "fmask=0077" "dmask=0077" "defaults" ];
  };

  swapDevices = [{
    device =
      "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S59ANMFNB30863T-part4";
    discardPolicy = "both";
    randomEncryption = {
      enable = true;
      # Enable trim on SSD; this has security implications
      allowDiscards = true;
    };
  }];

  # Fix podman on ZFS
  virtualisation.containers.storage.settings.storage.driver = "zfs";

  ###
  ### ZFS
  ###
  # NOTE: use latest Linux kernel that works with ZFS
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  # Regularly scrub ZFS pools (as reccomended)
  services.zfs.autoScrub.enable = true;

  # Automatically trim
  services.zfs.trim.enable = true;

  # Take snapshots automatically
  services.sanoid = {
    enable = true;
    datasets = {
      ### ROOT: npool - single-SSD root pool
      "npool/nixos/home" = { use_template = [ "hourly" ]; };
      "npool/nixos/home/dev" = { use_template = [ "daily" ]; };
      "npool/nixos/home/source" = { use_template = [ "daily" ]; };
      "npool/nixos/var" = { use_template = [ "hourly" ]; };

      ### rpool: 5-disk spinning rust pool
      "rpool/root/nixos" = { use_template = [ "hourly" ]; };
      "rpool/home" = { use_template = [ "hourly" ]; };
      "rpool/data" = { use_template = [ "hourly" ]; };
      "rpool/data/media" = { use_template = [ "daily" ]; };

      ### BACKUPS (also on 5-HDD pool)
      "rpool/backup" = {
        use_template = [ "backup" ];
        recursive = true;
      };
      "rpool/backup/time_machine" = {
        use_template = [ "backup" ];
        recursive = true;
      };
    };

    templates = {
      # Hourly: hourly backups
      "hourly" = {
        frequently = 0;
        hourly = 24;
        daily = 7;
        monthly = 3;
        yearly = 0;
        autosnap = true;
        autoprune = true;
      };
      # Daily: daily snapshots
      "daily" = {
        frequently = 0;
        hourly = 0;
        daily = 14;
        monthly = 3;
        yearly = 0;
        autosnap = true;
        autoprune = true;
      };
      # Backup: keep long-term
      "backup" = {
        frequently = 0;
        hourly = 0;
        daily = 14;
        monthly = 6;
        yearly = 0;
        autosnap = true;
        autoprune = true;
      };
    };
  };

  # Configure zed (ZFS Event Daemon) to send emails
  services.zfs.zed.settings = {
    ZED_DEBUG_LOG = "/tmp/zed.debug.log";
    ZED_EMAIL_ADDR = [ "root" ];
    ZED_EMAIL_PROG = "${pkgs.msmtp}/bin/msmtp";
    ZED_EMAIL_OPTS = "@ADDRESS@";

    ZED_NOTIFY_INTERVAL_SECS = 3600;
    ZED_NOTIFY_VERBOSE = true;

    ZED_USE_ENCLOSURE_LEDS = true;
    ZED_SCRUB_AFTER_RESILVER = true;
  };
  # this option does not work; will return error
  services.zfs.zed.enableMail = false;

  environment.systemPackages = with pkgs; [
    httm # Interactive, file-level Time Machine-like tool for ZFS/btrfs
    zpool-iostat-viz # "zpool iostats" for humans; find the slow parts of your ZFS pool
    ioztat # A storage load analysis tool for OpenZFS
    (pkgs.writeShellScriptBin "monitor-sanoid"
      ''
        ${config.systemd.services.sanoid.serviceConfig.ExecStart} --monitor-health
      '')
  ];
}
