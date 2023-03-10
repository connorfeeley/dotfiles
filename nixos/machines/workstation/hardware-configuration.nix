# FIXME: use device labels for interop
{ config, lib, pkgs, ... }: {
  boot.initrd.availableKernelModules =
    [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.extraModulePackages = [ ];

  boot.initrd.supportedFilesystems = [ "ext4" "zfs" ];
  boot.supportedFilesystems = [ "ext4" "ntfs" "zfs" ];

  boot.kernelParams = [
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the hourly) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp9s0.useDHCP = lib.mkHourly true;
  # networking.interfaces.tailscale0.useDHCP = lib.mkHourly true;
  # networking.interfaces.wlp7s0.useDHCP = lib.mkHourly true;

  ###
  ### ZFS
  ###
  networking.hostId = "5679a857";
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
      "npool/nixos/home" = {
        use_template = [ "hourly" ];
        recursive = true;
      };
      "npool/nixos/home/dev" = { use_template = [ "daily" ]; };
      "npool/nixos/home/source" = { use_template = [ "daily" ]; };
      "npool/nixos/var" = { use_template = [ "hourly" ]; };

      ### BOOT: bpool - single-SSD boot pool
      "bpool/nixos/home" = { use_template = [ "hourly" ]; };

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
      # Media: only snapshot daily
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
        hourly = 36;
        daily = 14;
        monthly = 6;
        yearly = 0;
        autosnap = true;
        autoprune = false;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    httm # Interactive, file-level Time Machine-like tool for ZFS/btrfs
    zpool-iostat-viz # "zpool iostats" for humans; find the slow parts of your ZFS pool
    ioztat # A storage load analysis tool for OpenZFS
  ];
}
