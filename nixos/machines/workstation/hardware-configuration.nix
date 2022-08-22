# FIXME: use device labels for interop
{
  config,
  lib,
  pkgs,
  ...
}: {
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
  boot.initrd.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  # LUKS-encrypted Linux boot
  boot.initrd.luks.devices.luksroot = {
    device = "/dev/disk/by-uuid/c1b38fbf-1786-4d0d-bfed-eb4bc15570f9";
    preLVM = true;
    allowDiscards = true;
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/b13cc249-6db1-41c3-bd28-9213dbd5b773";
      fsType = "ext4";
    };

  fileSystems."/home" =
    {
      device = "/dev/disk/by-uuid/9d3dd037-d860-40e1-b053-adf05f7f7dc1";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/3AE2-DDE7";
      fsType = "vfat";
    };

  fileSystems."/mnt/ssd" =
    {
      device = "/dev/disk/by-uuid/f08114d8-bff1-4c63-9e85-3d3aa09aca50";
      fsType = "ext4";
    };

  # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
  #      vulnerabilities. Don't copy this blindly! And especially not for
  #      mission critical or server/headless builds exposed to the world.
  boot.kernelParams = [
    "mitigations=off"
  ];

  swapDevices =
    [{ device = "/dev/disk/by-uuid/c1b79739-30a2-45fd-b238-b54049525d00"; }];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp9s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.tailscale0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp7s0.useDHCP = lib.mkDefault true;
}
