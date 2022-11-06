{ config
, lib
, primaryUser
, modulesPath
, pkgs
, ...
}:
let
  inherit (lib)
    mkDefault
    mkForce
    ;
in
{
  # imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];
  imports = [
    (modulesPath + "/installer/netboot/netboot-minimal.nix")
  ];

  # nixos-generate
  boot.loader.grub = lib.mkDefault {
    enable = true;
    version = 2;
    devices = [ "/dev/sda" ];
  };

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  swapDevices = [ ];
  hardware.cpu.amd.updateMicrocode = mkDefault config.hardware.enableRedistributableFirmware;

  fileSystems."/" = {
    # N.B. While nixos-generate-config will set this to a UUID by default, the
    # UUID of a disk on Hetzner Cloud appears to change if the server is
    # rebuilt. We know the root filesystem should always point to this
    # partition, so it's safer to point directly there.
    device = "/dev/sda1";
    fsType = "ext4";
  };

  services.openssh = lib.mkDefault {
    enable = true;
    openFirewall = true;
    # Authorized keys and permitRootLogin are set in ssh-host profile
  };

  # environment.enableAllTerminfo = mkForce false;

  # TODO: port to nixos:core
  # documentation = let enable = false; in mkForce {
  #   enable = enable;
  #   doc.enable = enable;
  #   dev.enable = enable;
  #   nixos.enable = enable;
  #   man = {
  #     enable = enable;
  #     generateCaches = false;
  #     # TODO: pick one of man-db or mandoc as viewer
  #     # man-db.enable = true;
  #     # mandoc.enable = true;
  #   };
  # };
}
