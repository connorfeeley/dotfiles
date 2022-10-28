moduleArgs @ { config
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
}
