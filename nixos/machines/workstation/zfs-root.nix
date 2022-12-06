{ ...
}: {
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.efiInstallAsRemovable = false;
  boot.loader.generationsDir.copyKernels = true;
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.copyKernels = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.gfxmodeEfi = "2560x1440";
  boot.loader.grub.zfsSupport = true;
  boot.loader.grub.extraPrepareConfig = ''
    mkdir -p /boot/efis
    for i in  /boot/efis/*; do mount $i ; done

    mkdir -p /boot/efi
    mount /boot/efi
  '';
  boot.loader.grub.extraInstallCommands = ''
    ESP_MIRROR=$(mktemp -d)
    cp -r /boot/efi/EFI $ESP_MIRROR
    for i in /boot/efis/*; do
     cp -r $ESP_MIRROR/EFI $i
    done
    rm -rf $ESP_MIRROR
  '';
  boot.loader.grub.devices = [
    "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S59ANMFNB30863T"
  ];

  fileSystems."/" =
    {
      device = "npool/nixos/root";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

  fileSystems."/home" =
    {
      device = "npool/nixos/home";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

  fileSystems."/var/lib" =
    {
      device = "npool/nixos/var/lib";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

  fileSystems."/var/log" =
    {
      device = "npool/nixos/var/log";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

  fileSystems."/boot" =
    {
      device = "bpool/nixos/root";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

  # EFI partion
  fileSystems."/boot/efi" =
    {
      device = "/dev/disk/by-uuid/E342-9852"; # "-part1"
      fsType = "vfat";
      options = [ "x-systemd.idle-timeout=1min" "x-systemd.automount" "noauto" ];
    };

  swapDevices = [
    {
      device = "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_1TB_S59ANMFNB30863T-part4";
      discardPolicy = "both";
      randomEncryption = {
        enable = true;
        # Enable trim on SSD; this has security implications
        allowDiscards = true;
      };
    }
  ];

}
