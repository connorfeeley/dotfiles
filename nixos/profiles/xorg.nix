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

  services.xserver = {
    enable = true;
    layout = "dvorak";
    libinput.enable = false;
    enableTCP = true; # allow X server to accept TCP conn.
    exportConfiguration = true; # symlink conf under /etc/X11/xorg.conf
    updateDbusEnvironment = true; # update the DBus activation environment

    xkbOptions = "ctrl:nocaps,ctrl:swap_lalt_lctl";

    displayManager.startx.enable = false;

    displayManager = {
      # Log in automatically
      # autoLogin = {
      #   enable = config.dotfield.guardian.autoLogin;
      #   user = config.dotfield.guardian.username;
      # };

      sessionCommands =
        let
          xmodmap = pkgs.writeText "xkb-layout" ''
            !Swap control and caps lock
            clear Lock
            keysym Caps_Lock = Escape
            keysym Escape = Caps_Lock
            add Lock = Caps_Lock

            !Swap left alt and left control
            clear control
            clear mod1
            keycode 37 = Alt_L Meta_L
            keycode 105 = Alt_R
            keycode 64 = Control_L
            keycode 108 = Control_R
            add control = Control_L Control_R
            add mod1 = Alt_L Meta_L Alt_R
          '';
        in
        ''
          # ${pkgs.xorg.xmodmap}/bin/xmodmap ${xmodmap}

          # Fix keyring unlock
          ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
        '';
    };

    # WM-only sessions usually don't handle XDG autostart files by default.
    desktopManager.runXdgAutostartIfNone = true;

    # Don't autosuspend from GDM
    displayManager.gdm.autoSuspend = false;
  };
}
