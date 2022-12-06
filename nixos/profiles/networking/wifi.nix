{ ...
}: {
  # Prevent stupid boot delays waiting for internet.
  # https://discourse.nixos.org/t/boot-faster-by-disabling-udev-settle-and-nm-wait-online/6339
  systemd.services.NetworkManager-wait-online.enable = false;
}
