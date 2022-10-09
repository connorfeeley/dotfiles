{
  config,
  lib,
  pkgs,
  ...
}: let
  envExtra = import ../env-init.sh.nix;
  shellAliases =
    (import ../abbrs.nix)
    // (import ../aliases.nix);
in {
  imports = [../common.nix];

  home.packages = with pkgs; [
    bash
  ];

  programs.bash = {
    inherit
      envExtra
      shellAliases
      ;

    enable = true;
    dotDir = ".config/bash";
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableAutosuggestions = true;

    history = {
      path = "${config.xdg.dataHome}/bash/history";
      expireDuplicatesFirst = false;
      extended = true; # Save timestamps
      ignoreDups = false; # Always insert into history
      ignoreSpace = true; # Prepend command with a space to skip history insertion

      # Infinite history
      save = 1000000000;
      size = 1000000000;
    };

    initExtraFirst = ''
      # Init starship when:
      # - TERM is not dumb (which it is over TRAMP)
      # AND :
      # - We are not in emacs
      # - We are not in an emacs vterm over TRAMP
      # echo "\$TERM = $TERM	\$INSIDE_EMACS = $INSIDE_EMACS"
      if [[ $TERM != "dumb" && ( -z $INSIDE_EMACS || "''${INSIDE_EMACS/*tramp*/tramp}" != "tramp") ]]; then
        eval "$(${pkgs.starship}/bin/starship init bash)"
      fi
    '';

    initExtra = ''
      # MacOS only: XQuartz
      if [ "$(uname)" = "Darwin" -a -n "$NIX_LINK" -a -f $NIX_LINK/etc/X11/fonts.conf ]; then
        export FONTCONFIG_FILE=$NIX_LINK/etc/X11/fonts.conf
      fi

      # Enable vi-mode outside of emacs; otherwise source vterm-specific configuration
      if [[ -z $INSIDE_EMACS ]]; then
        set -o vi
      else
        source ${pkgs.emacsPackages.vterm}/etc/emacs-vterm-bash.sh
      fi
    '';

    sessionVariables = {
      BASH_COMPLETION_USER_FILE = "${config.xdg.dataHome}/bash/completion";
    };
  };
}
