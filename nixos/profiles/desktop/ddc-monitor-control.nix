{ lib, config, pkgs, ... }:
let
  inherit (config.dotfield) guardian;

  monitors = {
    center = 1; # i2c-4
    left = 2; # i2c-5
    right = 3; # ic2-6
  };

  mkProfile = percentBrightness: ''
    <?xml version="1.0"?>
    <profile name="side-daytime" pnpid="SAM0C4D" version="1">
      <control address="0x10" value="0x${lib.toHexString percentBrightness}"/>
    </profile>
  '';

  daytimeProfile = pkgs.writeText "daytime-profile" (mkProfile 55);
  nighttimeProfile = pkgs.writeText "daytime-profile" (mkProfile 30);

  scriptDaytime = pkgs.writeShellScriptBin "monitor-daytime" ''
    # Control 0x10: brightness
    ${pkgs.ddccontrol}/bin/ddccontrol -r 0x10 -w 100 dev:/dev/i2c-4 # center monitor
    ${pkgs.ddccontrol}/bin/ddccontrol -r 0x10 -w  55 dev:/dev/i2c-5 # left monitor
    ${pkgs.ddccontrol}/bin/ddccontrol -r 0x10 -w  55 dev:/dev/i2c-6 # right monitor
  '';
  scriptNighttime = pkgs.writeShellScriptBin "monitor-nighttime" ''
    # Control 0x10: brightness
    ${pkgs.ddccontrol}/bin/ddccontrol -r 0x10 -w 33 dev:/dev/i2c-4 # center monitor
    ${pkgs.ddccontrol}/bin/ddccontrol -r 0x10 -w 33 dev:/dev/i2c-5 # left monitor
    ${pkgs.ddccontrol}/bin/ddccontrol -r 0x10 -w 33 dev:/dev/i2c-6 # right monitor
  '';
in
{
  boot.extraModulePackages = [ config.boot.kernelPackages.ddcci-driver ];
  boot.kernelModules = [ "i2c-dev" "ddcci_backlight" ];

  # Also provides 'gddccontrol' GUI
  services.ddccontrol.enable = true;

  # Add brightness scripts to environment
  environment.systemPackages = [ scriptDaytime scriptNighttime ];

  services.udev.extraRules = ''
    KERNEL=="i2c-[0-9]*", TAG+="uaccess"
  '';

  systemd.services."ddcci@" = {
    scriptArgs = "%i";
    script = ''
      echo Trying to attach ddcci to $1
      i=0
      id=$(echo $1 | cut -d "-" -f 2)
      if ${pkgs.ddcutil}/bin/ddcutil getvcp 10 -b $id; then
        echo ddcci 0x37 > /sys/bus/i2c/devices/$1/new_device
      fi
    '';
    serviceConfig.Type = "oneshot";
  };

  # Allow guardian user to access run ddcutil commands
  users.users.${guardian.username}.extraGroups = [ "i2c" ];
}
