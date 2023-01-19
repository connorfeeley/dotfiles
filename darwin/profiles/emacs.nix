{ config, pkgs, ... }:
let
  inherit (config.dotfield.guardian) username;

  # Primary user's HM configuration
  guardianConfig = config.home-manager.users."${username}";
in
{
  environment.variables = {
    DOOMDIR = "$XDG_CONFIG_HOME/doom";
    EMACSDIR = "$XDG_CONFIG_HOME/emacs";
  };

  homebrew = {
    taps = [{ name = "railwaycat/emacsmacport"; }];
    brews = [
      # {
      #   name = "emacs-mac";
      #   args = [
      #     "with-natural-title-bar"
      #     "with-starter"
      #     # "with-mac-metal"
      #     "with-native-compilation"
      #     "with-xwidgets"
      #   ];
      # }
      # Emacs -> :lang org (macOS only)
      { name = "pngpaste"; }
      { name = "coreutils"; }
    ];
  };

  services.emacs = {
    enable = true;
    package = guardianConfig.programs.emacs.package;
    additionalPath = [ "/Users/${config.dotfield.guardian.username}/.config/emacs/bin" ];
  };
}
