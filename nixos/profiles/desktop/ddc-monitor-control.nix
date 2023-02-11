{ lib, config, pkgs, ... }:
let
  inherit (config.dotfield) guardian;

  monitors = {
    # Dell Ultrawide
    U2917W = {
      busNumbers = [ 4 ];
      brightness.daytime = 100;
      brightness.nighttime = 40;
    };
    # Samsung UHD (TN panel; brightness at night: the light of 1000 suns; flat)
    U28E590 = {
      busNumbers = [ 5 6 ];
      brightness.daytime = 55;
      brightness.nighttime = 33;
    };
    # Samsung UHD (VA panel; brightness during the day: similar to the brightness of a sketchy alley at night; curved)
    U32R59x = {
      busNumbers = [ 7 ];
      brightness.daytime = 100;
      brightness.nighttime = 55;
    };
  };

  mkProfile = percentBrightness: ''
    <?xml version="1.0"?>
    <profile name="side-daytime" pnpid="SAM0C4D" version="1">
      <control address="0x10" value="0x${lib.toHexString percentBrightness}"/>
    </profile>
  '';

  daytimeProfile = pkgs.writeText "daytime-profile" (mkProfile 55);
  nighttimeProfile = pkgs.writeText "daytime-profile" (mkProfile 30);

  setBrightness = i2cBus: brightness: ''
    # Control 0x10: brightness
    ${pkgs.ddccontrol}/bin/ddccontrol -r 0x10 -w ${toString brightness} dev:/dev/i2c-${toString i2cBus}
  '';

  scriptDaytime = with monitors; pkgs.writeShellScriptBin "monitor-daytime" ''
    ${lib.concatMapStringsSep "\n"
    (bus: setBrightness bus U2917W.brightness.daytime) U2917W.busNumbers}

    ${lib.concatMapStringsSep "\n"
      (bus: setBrightness bus U28E590.brightness.daytime) U28E590.busNumbers}

    ${lib.concatMapStringsSep "\n"
    (bus: setBrightness bus U32R59x.brightness.daytime) U32R59x.busNumbers}
  '';

  scriptNighttime = with monitors; pkgs.writeShellScriptBin "monitor-nighttime" ''
    ${lib.concatMapStringsSep "\n"
    (bus: setBrightness bus U2917W.brightness.nighttime) U2917W.busNumbers}

    ${lib.concatMapStringsSep "\n"
    (bus: setBrightness bus U28E590.brightness.nighttime) U28E590.busNumbers}

    ${lib.concatMapStringsSep "\n"
    (bus: setBrightness bus U32R59x.brightness.nighttime) U32R59x.busNumbers}
  '';
in {
  # FIXME: broken with current kernel
  # boot.extraModulePackages = [ config.boot.kernelPackages.ddcci-driver ];
  boot.kernelModules = [ "i2c-dev" ];

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
