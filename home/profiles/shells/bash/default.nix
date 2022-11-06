{ config
, lib
, pkgs
, ...
}:
let
  shellAliases =
    (import ../abbrs.nix)
    // (import ../aliases.nix);
in
{
  imports = [ ../common.nix ];

  home.packages = with pkgs; [
    bashInteractive
  ];

  programs.starship.enableBashIntegration = false;

  programs.bash = {
    inherit
      shellAliases
      ;

    enable = true;
    enableCompletion = true;
    enableVteIntegration = false;

    historyFile = "${config.xdg.dataHome}/bash/history";
    historyControl = [ "ignorespace" ];

    # Infinite history
    historyFileSize = 1000000000;
    historySize = 1000000000;

    initExtra = ''
      # # Source vterm-specific configuration
      [[ -n $EMACS_VTERM_PATH ]] && source $EMACS_VTERM_PATH/etc/emacs-vterm-bash.sh

      # MacOS only: XQuartz
      if [ "$(uname)" = "Darwin" -a -n "$NIX_LINK" -a -f $NIX_LINK/etc/X11/fonts.conf ]; then
        export FONTCONFIG_FILE=$NIX_LINK/etc/X11/fonts.conf
      fi
    '';

    sessionVariables = {
      BASH_COMPLETION_USER_FILE = "${config.xdg.dataHome}/bash/completion";
    };
  };
}
