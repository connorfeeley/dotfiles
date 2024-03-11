# FIXME: use device labels for interop
{ config, lib, pkgs, inputs, ... }: {
  # Windows
  boot.loader.grub.extraEntries = ''
    menuentry "Windows" {
      insmod part_gpt
      insmod fat
      insmod search_fs_uuid
      insmod chain
      search --fs-uuid --set=root $FS_UUID
      chainloader /EFI/Microsoft/Boot/bootmgfw.efi
    }
  '';

  boot.initrd.availableKernelModules =
    [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];

  boot.initrd.supportedFilesystems = [ "ext4" "zfs" ];
  boot.supportedFilesystems = [ "ext4" "ntfs" "zfs" ];

  boot.kernelParams = [
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the hourly) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp9s0.useDHCP = lib.mkHourly true;
  # networking.interfaces.tailscale0.useDHCP = lib.mkHourly true;
  # networking.interfaces.wlp7s0.useDHCP = lib.mkHourly true;
}
