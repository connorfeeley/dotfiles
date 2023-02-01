{ config
, lib
, pkgs
, ...
}: {
  hardware.acpilight.enable = true;

  # NOTE: see https://source.mcwhirter.io/craige/mio-ops/src/branch/consensus/profiles/xmonad.nix
  # for an example config.
  programs = {
    ssh.askPassword = pkgs.lib.mkForce "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";
    kdeconnect.enable = true;
    dconf.enable = true;
  };

  xdg.portal.enable = true;

  # Interception enables remapping keys at a lower level than xmodmap, setxkbmap, or xcape.
  # These mappings also apply in the TTY.
  services.interception-tools =
    let
      # Map caps lock to:
      # - ESC when tapped
      # - LCTRL when held
      dualFunctionKeysConfig = pkgs.writeText "dual-function-keys.yaml" ''
        MAPPINGS:
          # CAPS (tap) -> ESC, CAPS (hold) -> LCTRL
          - KEY: KEY_CAPSLOCK
            TAP: KEY_ESC
            HOLD: KEY_LEFTCTRL

          # Swap LALT with LCTRL
          - KEY: KEY_LEFTALT
            TAP: KEY_LEFTCTRL
            HOLD: KEY_LEFTCTRL
            HOLD_START: BEFORE_CONSUME

          # Swap LCTRL with LALT
          - KEY: KEY_LEFTCTRL
            TAP: KEY_LEFTALT
            HOLD: KEY_LEFTALT
            HOLD_START: BEFORE_CONSUME
      '';
    in
    {
      enable = true;
      plugins = [ pkgs.interception-tools-plugins.dual-function-keys ];
      udevmonConfig = ''
        - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.dual-function-keys}/bin/dual-function-keys -c ${dualFunctionKeysConfig} | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
          DEVICE:
            LINK: .*-event-kbd
      '';
    };

  services.xserver = {
    enable = true;
    layout = "dvorak";
    libinput.enable = false;
    enableTCP = true; # allow X server to accept TCP conn.
    updateDbusEnvironment = true; # update the DBus activation environment

    exportConfiguration = true; # symlink conf under /etc/X11/xorg.conf

    # Mapped from left to right
    # Affects /etc/X11/xorg.conf
    xrandrHeads = [
      {
        output = "DP-0";
        monitorConfig = ''
          DisplaySize 607 345
          Option      "DPMS"
        '';
      }
      {
        output = "HDMI-0";
        primary = true;
        monitorConfig = ''
          DisplaySize 697 392
          Option      "DPMS"
        '';
      }
      {
        output = "DP-2";
        monitorConfig = ''
          DisplaySize 607 345
          Option      "DPMS"
        '';
      }
    ];

    displayManager.startx.enable = false;

    displayManager = {
      # Log in automatically
      autoLogin = {
        enable = config.dotfield.guardian.autoLogin;
        user = config.dotfield.guardian.username;
      };

      sessionCommands = ''
        # Fix keyring unlock
        ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
      '';
    };

    # WM-only sessions usually don't handle XDG autostart files by default.
    desktopManager.runXdgAutostartIfNone = true;

    # Don't autosuspend from GDM
    displayManager.gdm.autoSuspend = false;
  };

  programs.light.enable = true; # Backlight control for users in the 'video' group
}
