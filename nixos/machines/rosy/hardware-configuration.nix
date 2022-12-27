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
    { device = "/dev/disk/by-uuid/c31a1f64-1ce9-4c7a-9b8f-4bffd490176a";
      fsType = "ext4";
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/2B2E-B964";
      fsType = "vfat";
    };

  swapDevices = [ ];

  hardware.parallels.enable = true;
}
