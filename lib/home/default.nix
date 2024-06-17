moduleArgs@{ config, lib, ... }:
let
  inherit (config.xdg) configHome;

  sysLib = moduleArgs.osConfig.lib.dotfiles or { };

  this = config.lib.dotfiles;
  defaultUsername = "cfeeley";
in
{
  lib.dotfiles = rec {
    fsPath = toString "${configHome}/dotfiles";
    srcPath = ../../.;
    userConfigPath = srcPath + "/home/users/${defaultUsername}/config";

    features = rec {
      hasPragPro =
        lib.strings.hasPrefix "PragmataPro" config.theme.font.mono.family;
      hasSway = config.wayland.windowManager.sway.enable;
      hasTwm = sysLib.sys.hasTwm or hasSway;
      hasWayland = sysLib.sys.hasWayland or hasSway;
    };

    # FIXME: move this back to a module -- guardian
    whoami = rec {
      firstName = "Connor";
      lastName = "Feeley";
      fullName = "${firstName} ${lastName}";
      email = "git@" + domain;
      domain = "cfeeley.org";
      githubUserName = "connorfeeley";
      pgpPublicKey = "0x77CB2390C53B4E5B";
      pgpKeygrip = "88EFF3DC7355A8AE37CE79ECF250098D909F3544";
    };

    emacs = rec {
      profilesBase = "emacs/profiles";
      profilesPath = "${this.userConfigPath}/${profilesBase}";
    };
  };
}
