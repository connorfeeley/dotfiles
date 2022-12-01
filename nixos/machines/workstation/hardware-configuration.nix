# FIXME: use device labels for interop
{ config
, lib
, pkgs
, ...
}: {
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  boot.initrd.supportedFilesystems = [ "ext4" "zfs" ];
  boot.supportedFilesystems = [ "ext4" "ntfs" "zfs" ];

  # LUKS-encrypted Linux boot
  # boot.initrd.luks.devices.luksroot = {
  #   device = "/dev/disk/by-uuid/c1b38fbf-1786-4d0d-bfed-eb4bc15570f9";
  #   preLVM = true;
  #   allowDiscards = true;
  #   bypassWorkqueues = true;
  # };
  #
  # fileSystems."/" =
  #   {
  #     device = "/dev/disk/by-uuid/b13cc249-6db1-41c3-bd28-9213dbd5b773";
  #     fsType = "ext4";
  #   };
  #
  # fileSystems."/home" =
  #   {
  #     device = "/dev/disk/by-uuid/9d3dd037-d860-40e1-b053-adf05f7f7dc1";
  #     fsType = "ext4";
  #   };
  #
  # fileSystems."/boot" =
  #   {
  #     device = "/dev/disk/by-uuid/86A8-3AD7";
  #     fsType = "vfat";
  #   };

  # fileSystems."/mnt/ssd" =
  #   {
  #     device = "/dev/disk/by-uuid/f08114d8-bff1-4c63-9e85-3d3aa09aca50";
  #     fsType = "ext4";
  #   };

  boot.kernelParams = [
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"

    # Max ARC (Adaptive Replacement Cache) size: 12GB
    "zfs.zfs_arc_max=19327352832" # 18 GB
  ];

  # swapDevices =
  #   [{ device = "/dev/disk/by-uuid/c1b79739-30a2-45fd-b238-b54049525d00"; }];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp9s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.tailscale0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp7s0.useDHCP = lib.mkDefault true;

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
      "npool/nixos/home" = { use_template = [ "production" ]; };

      "rpool/root/nixos" = { use_template = [ "production" ]; };
      "rpool/home" = { use_template = [ "production" ]; };
      "rpool/data" = { use_template = [ "production" ]; };
      "rpool/data/media" = { use_template = [ "production" ]; };
      "rpool/backup" = { use_template = [ "production" ]; };
    };

    templates = {
      "production" = {
        frequently = 0;
        hourly = 36;
        daily = 30;
        monthly = 3;
        yearly = 0;
        autosnap = true;
        autoprune = true;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    # Interactive, file-level Time Machine-like tool for ZFS/btrfs
    httm
  ];

  boot.zfs.extraPools = [ "rpool" ];
}
