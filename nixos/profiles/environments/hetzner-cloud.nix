{ config
, lib
, primaryUser
, modulesPath
, pkgs
, ...
}:
let
  inherit (lib)
    mkOption
    mkDefault
    mkForce
    types
    ;
in
{
  # Causes the stupid kernel documentation to get built.
  # imports = [
  #   (modulesPath + "/installer/netboot/netboot-minimal.nix")
  # ];

  options = {
    environments.isHetzner = mkOption {
      type = types.bool;
      description = "R/O (ish) flag indicating to home-manager that the target environment is a Hetzner VM.";
      default = true;
      readOnly = true;
    };
  };

  config = {
    # nixos-generate
    boot.loader.grub = {
      enable = lib.mkDefault true;
      version = lib.mkDefault 2;
      devices = lib.mkDefault [ "/dev/sda" ];
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
  };
}
