{ config, lib, pkgs, modulesPath, ... }:
let
  useAppleVirtualization = false; # false for QMEU
in
{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "virtio_pci" "xhci_pci" "usb_storage" "usbhid" "virtiofs" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.cleanTmpDir = true;

  fileSystems."/" =
    {
      device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };

  # Mount host's shared directory to /run/share
  fileSystems."/run/share" = if (!useAppleVirtualization) then {
    # For virtfs (QEMU)
    device = "share";
    fsType = "9p";
    options = [ "trans=virtio" "version=9p2000.L" ];

    # Manually:
    # sudo mkdir /media/share
    # sudo mount -t 9p -o trans=virtio share /media/share -oversion=9p2000.L
  } else {
    # For virtiofs (apple virtualization)
    device = "share";
    fsType = "virtiofs";

    # Manually:
    # sudo mkdir /run/share
    # sudo mount -t virtiofs share /run/share/
  };

  # Emulate x86_64-linux with QEMU
  boot.binfmt.emulatedSystems = [ "x86_64-linux" ];

  nix.settings.extra-platforms = [ "x86_64-linux" ];
}
