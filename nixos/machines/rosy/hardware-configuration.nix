{ config, lib, pkgs, modulesPath, ... }:
let
  useAppleVirtualization = true; # false for QMEU
in
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

  ### === rosetta ================================================================
  # https://xyno.space/post/nixos-utm-rosetta

  boot.binfmt.emulatedSystems = [ "x86_64-linux" ];

  fileSystems."/run/rosetta" = lib.optionals useAppleVirtualization {
    device = "rosetta";
    fsType = "virtiofs";
  };
  nix.settings.extra-platforms = lib.optionals useAppleVirtualization [ "x86_64-linux" ];
  nix.settings.extra-sandbox-paths = lib.optionals useAppleVirtualization [ "/run/rosetta" "/run/binfmt" ];
  boot.binfmt.registrations."rosetta" = lib.optionals useAppleVirtualization {
    # based on https://developer.apple.com/documentation/virtualization/running_intel_binaries_in_linux_vms_with_rosetta#3978495
    interpreter = "/run/rosetta/rosetta";
    fixBinary = true;
    wrapInterpreterInShell = false;
    matchCredentials = true;
    magicOrExtension = ''\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x3e\x00'';
    mask = ''\xff\xff\xff\xff\xff\xfe\xfe\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'';
  };
}
