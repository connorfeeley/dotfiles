{ config
, lib
, pkgs
, ...
}: {
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

    displayManager = {
      # Enable the LightDM display manager
      lightdm.enable = true;

      # Log in automatically
      autoLogin = {
        enable = true;
        user = config.dotfield.guardian.username;
      };

      # Fix keyring unlock
      sessionCommands = ''
        ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
      '';

      # Pre-select HM xsession
      defaultSession = "xsession";
    };

    ###
    ### XMonad (via home-manager)
    ###
    desktopManager.session = [{
      # Run xmonad session from home-manager
      name = "xsession";
      start = ''
        ${pkgs.zsh} $HOME/.xsession &
        waitPID=$!
      '';
    }];

    ###
    ### KDE
    ###
    desktopManager.plasma5 = {
      enable = false;
      supportDDC = true;
      useQtScaling = true;
      runUsingSystemd = true;
    };
  };
}
