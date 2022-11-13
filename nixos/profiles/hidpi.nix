{ config
, lib
, pkgs
, ...
}: {
  # hardware.video.hidpi.enable = true;

  # Larger console font
  console.font = lib.mkDefault "latarcyrheb-sun32";

  # Also use large console font in initrd
  console.earlySetup = true;

  # Try use maximum resolution in systemd-boot
  boot.loader.systemd-boot.consoleMode = lib.mkDefault "max";
}
