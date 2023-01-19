{ config
, ...
}:
{
  services.smartd = {
    enable = true;
    autodetect = true;
    extraOptions = [
      "-A /var/log/smartd/"
      # "--interval=3600"
      "--interval=60"
    ];
  };

  # Allow users in the 'disk' group to access NVME character devices (char device: more raw/low-level; block device: storage).
  # Required for smartctl prometheus exporter.
  services.udev.extraRules = ''
    SUBSYSTEM=="nvme", KERNEL=="nvme[0-9]*", GROUP="disk"
  '';
}
