{ lib, ... }:
let
  # useAppleVirtualization = false; # false for QMEU
in
{
  system.stateVersion = "22.11";

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "nvme" "usb_storage" "usbhid" "sr_mod" ] ++ [ "virtio_pci" "virtiofs" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.cleanTmpDir = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/1040b84b-91b4-4a25-b4d3-cf1493a8a5b6";
      fsType = "ext4";
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/CEAA-2742";
      fsType = "vfat";
    };

  swapDevices = [ ];

  hardware.parallels.enable = true;
  # nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "prl-tools" ];

  # Mount host's shared directory to /run/share
  # fileSystems."/run/share" =
  #   if (!useAppleVirtualization) then {
  #     # For virtfs (QEMU)
  #     device = "share";
  #     fsType = "9p";
  #     options = [ "trans=virtio" "version=9p2000.L" ];

  #     # Manually:
  #     # sudo mkdir /media/share
  #     # sudo mount -t 9p -o trans=virtio share /media/share -oversion=9p2000.L
  #   } else {
  #     # For virtiofs (apple virtualization)
  #     device = "share";
  #     fsType = "virtiofs";

  #     # Manually:
  #     # sudo mkdir /run/share
  #     # sudo mount -t virtiofs share /run/share/
  #   };
}
