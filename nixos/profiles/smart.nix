{ config
, ...
}:
{
  services.smartd = {
    enable = true;
    autodetect = true;
  };

  # Allow users in the 'disk' group to access NVME character devices (char device: more raw/low-level; block device: storage).
  # Required for smartctl prometheus exporter.
  services.udev.extraRules = ''
    SUBSYSTEM=="nvme", KERNEL=="nvme[0-9]*", GROUP="disk"
  '';
}
