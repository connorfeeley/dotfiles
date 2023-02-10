{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux;

  configFile = pkgs.writeText "Barrier.conf" ''
    section: screens
      workstation:
        halfDuplexCapsLock = false
        halfDuplexNumLock = false
        halfDuplexScrollLock = false
        xtestIsXineramaUnaware = false
        preserveFocus = false
        switchCorners = none
        switchCornerSize = 0
      MacBook-Pro:
        halfDuplexCapsLock = false
        halfDuplexNumLock = false
        halfDuplexScrollLock = false
        xtestIsXineramaUnaware = false
        preserveFocus = false
        switchCorners = none
        switchCornerSize = 0
    end

    section: aliases
    end

    section: links
      workstation:
        down = MacBook-Pro
      MacBook-Pro:
        up = workstation
    end

    section: options
      relativeMouseMoves = false
      screenSaverSync = true
      win32KeepForeground = false
      clipboardSharing = true
      switchDoubleTap = 250
      switchCorners = none +top-left +top-right +bottom-left +bottom-right
      switchCornerSize = 0
      keystroke(Super+PageDown) = switchInDirection(down)
      keystroke(Super+PageUp) = switchInDirection(up)
      keystroke(Super+Control+Return) = toggleScreen
    end
  '';
in
{
  services.input-leap.server = {
    enable = true;
    autoStart = true;
    checkClientCert = false;
    address = ":24800";
    screenName = "workstation";
    inherit configFile;
  };
}
