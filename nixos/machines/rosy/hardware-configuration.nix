{ config, lib, pkgs, modulesPath, ... }:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "virtio_pci" "xhci_pci" "usb_storage" "usbhid" "virtiofs" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # TODO: add swap device:
  # /dev/sda2: LABEL="swap" UUID="d660c175-9640-40e0-a606-f633b8d762a5" TYPE="swap" PARTLABEL="primary" PARTUUID="a5a1cad4-7719-4434-aaa8-53c9df4ad884"
  # swapDevices = [ ];

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/af964634-e7e2-4b32-8be8-11f2b75c540f";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/EFF3-2E4B";
      fsType = "vfat";
    };

  # Mount host's shared directory to /run/share
  fileSystems."/run/share" = {
    neededForBoot = true;
    device = "share";
    # For virtfs (QEMU)
    fsType = "9p";
    options = [ "trans=virtio" "version=9p2000.L" "rw" "_netdev" "nofail" ];

    # For virtiofs (apple virtualization)
    # fsType = "virtiofs";
  };

  # NOTE: emulating x86_64 for now - try rosetta+apple virtualization once it's more stable
  # fileSystems."/run/rosetta" = {
  #   device = "rosetta";
  #   fsType = "virtiofs";
  # };
  # nix.settings.extra-platforms = [ "x86_64-linux" ];
  # nix.settings.extra-sandbox-paths = [ "/run/rosetta" "/run/binfmt" ];
  # boot.binfmt.registrations."rosetta" = {
  #   # based on https://developer.apple.com/documentation/virtualization/running_intel_binaries_in_linux_vms_with_rosetta#3978495
  #   interpreter = "/run/rosetta/rosetta";
  #   fixBinary = true;
  #   wrapInterpreterInShell = false;
  #   matchCredentials = true;
  #   magicOrExtension = ''\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x3e\x00'';
  #   mask = ''\xff\xff\xff\xff\xff\xfe\xfe\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'';
  # };

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp0s1.useDHCP = lib.mkDefault true;
}
