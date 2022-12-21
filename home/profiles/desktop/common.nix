{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in
lib.mkIf isLinux {
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    extraConfig = {
      XDG_DEV_DIR = "$HOME/Developer";
      XDG_MAIL_DIR = config.accounts.email.maildirBasePath or "$HOME/Mail";
      XDG_PROJECTS_DIR = "$HOME/Projects";
    };
  };

  qt.enable = true;

  # TODO
  # xdg.desktopEntries = ...
  # xdg.mime = ...
  # xdg.mimeApps = ...

  home.pointerCursor = {
    package = pkgs.gnome.gnome-themes-extra; # Cursor package
    size = 32;
    name = "Adwaita"; # Name within package
    gtk.enable = true;
    x11.enable = true;
  };

  services = {
    # Tap caps-lock to send ESC; hold for L_CTRL
    xcape = {
      enable = true;
      mapExpression = {
        Control_L = "Escape";
      };
      timeout = 200;
    };
  };
}
