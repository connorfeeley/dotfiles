{ config, options, lib, pkgs, ... }:
# Don't configure ZFS for VMs
lib.mkIf (!options.virtualisation ? qemu) {
  boot.loader.efi.efiSysMountPoint = "/boot";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.generationsDir.copyKernels = true;
  # boot.loader.grub.enable = true;
  # boot.loader.grub.copyKernels = true;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.gfxmodeEfi = "2560x1440";
  # boot.loader.grub.zfsSupport = true;
  # boot.loader.grub.extraPrepareConfig = ''
  #   mkdir -p /boot/efis
  #   for i in  /boot/efis/*; do mount $i ; done

  #   mkdir -p /boot/efi
  #   mount /boot/efi
  # '';
  # boot.loader.grub.extraInstallCommands = ''
  #   ESP_MIRROR=$(${pkgs.coreutils}/bin/mktemp -d)
  #   ${pkgs.coreutils}/bin/cp -r /boot/efi/EFI $ESP_MIRROR
  #   for i in /boot/efis/*; do
  #    ${pkgs.coreutils}/bin/cp -r $ESP_MIRROR/EFI $i
  #   done
  #   ${pkgs.coreutils}/bin/rm -rf $ESP_MIRROR
  # '';
  # boot.loader.grub.devices =
  #   [ "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S59ANMFNB30863T" ];

  boot.kernelParams = [
    # https://github.com/openzfs/zfs/issues/10255#issuecomment-1067415974
    # Max ARC (Adaptive Replacement Cache) size: 18GB
    "zfs.zfs_arc_max=${toString (4 * 1024 * 1024 * 1024)}" # 4 GB

    # Target number of bytes the ARC should leave as free memory on the system
    "zfs.zfs_arc_sys_free=${toString (3 * 1024 * 1024 * 1024)}" # 3 GiB
  ];

  boot.zfs.extraPools = lib.optionals (!config.nixos-vm.enable) [ "rpool" ];

  fileSystems."/" = {
    device = "npool/nixos/root";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/home" = {
    device = "npool/nixos/home";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/var/lib" = {
    device = "npool/nixos/var/lib";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/var/log" = {
    device = "npool/nixos/var/log";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  # fileSystems."/boot" = {
  #   device = "bpool/nixos/root";
  #   fsType = "zfs";
  #   options = [ "zfsutil" "X-mount.mkdir" ];
  # };

  # EFI partion
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/E342-9852"; # "-part1"
    fsType = "vfat";
    options = [ "fmask=0077" "dmask=0077" "defaults" ];
  };

  swapDevices = [{
    device =
      "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S59ANMFNB30863T-part4";
    discardPolicy = "both";
    randomEncryption = {
      enable = true;
      # Enable trim on SSD; this has security implications
      allowDiscards = true;
    };
  }];

  # Fix podman on ZFS
  virtualisation.containers.storage.settings.storage.driver = "zfs";
}
