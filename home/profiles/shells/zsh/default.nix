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
    zsh
    pure-prompt
  ];

  # Must be disabled for emacs-vterm integration to work.
  # Integration is handled manually in zsh.initExtra.
  programs.starship.enableZshIntegration = false;

  programs.zsh = {
    inherit
      shellAliases
      ;

    enable = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    # NOTE: conflicts with 'zdharma/fast-syntax-highlighting'
    enableSyntaxHighlighting = false;
    enableAutosuggestions = true;
    enableVteIntegration = true;

    defaultKeymap = "emacs"; # One of "emacs", "vicmd", "viins"

    history = {
      path = "${config.xdg.dataHome}/zsh/history";
      expireDuplicatesFirst = false;
      extended = true; # Save timestamps
      ignoreDups = false; # Always insert into history
      ignoreSpace = true; # Prepend command with a space to skip history insertion

      # Infinite history
      save = 1000000000;
      size = 1000000000;
    };
    historySubstringSearch.enable = true;

    zplug = {
      enable = true;
      plugins = [
        # { name = ""; tags = ""; }
        { name = "zdharma-continuum/fast-syntax-highlighting"; }
        { name = "zsh-users/zsh-completions"; }
        { name = "hlissner/zsh-autopair"; }
        { name = "agkozak/zsh-z"; }
        { name = "scriptingosx/mac-zsh-completions"; }
        { name = "marzocchi/zsh-notify"; }
      ];
    };

    # This is the top of $ZDOTDIR/.zshrc
    initExtraFirst = ''
      if [[ "$TERM" == "dumb" ]]; then
        unsetopt zle
        unsetopt prompt_cr
        unsetopt prompt_subst
        unsetopt zle_bracketed_paste
        if whence -w precmd >/dev/null; then
            unfunction precmd
        fi
        if whence -w preexec >/dev/null; then
            unfunction preexec
        fi
        unset RPROMPT
        unset RPS1
        PS1='$ '
        return
      fi
    '';

    # Before plugin init in $ZDOTDIR/.zshrc
    initExtraBeforeCompInit = ''
      # # Init completion manually
      # autoload compinit; compinit -u
    '';

    # After plugin init and history init in $ZDOTDIR/.zshrc
    # Followed by zoxide, command-not-found, direnv, GPG, aliases init.
    # Finally, followed by zsh-syntax-highlighting.
    initExtra = ''
      ### Starship
      # eval "$(${pkgs.starship}/bin/starship init zsh)"

      ### Pure
      autoload -U promptinit; promptinit
      prompt off
      PURE_PROMPT_SYMBOL="λ"
      prompt pure

      # Use vi-mode when:
      # - TERM is not dumb (which it is over TRAMP)
      # AND:
      # - We are not in emacs
      # - We are not in an emacs vterm over TRAMP
      # if [[ $TERM != "dumb" && ( -z $INSIDE_EMACS || "''${INSIDE_EMACS/*tramp*/tramp}" != "tramp") ]]; then

      if [[ $TERM != "dumb" && -z $INSIDE_EMACS ]]; then
        # Vi keybindings
        source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
      else
        # # Source vterm-specific configuration
        [[ -n $EMACS_VTERM_PATH ]] && source ${pkgs.emacsPackages.vterm}/share/emacs/site-lisp/elpa/vterm-*/etc/emacs-vterm-zsh.sh
        source ${pkgs.dotfield-config}/zsh/vterm.zsh
      fi
      # echo "\$TERM = $TERM	\$INSIDE_EMACS = $INSIDE_EMACS"

      # bind DEL to delete-char  make `vterm-send-delete` delete char (on darwin, at least)
      bindkey "\e[3~" delete-char

      # MacOS only: XQuartz
      if [ "$(uname)" = "Darwin" -a -n "$NIX_LINK" -a -f $NIX_LINK/etc/X11/fonts.conf ]; then
        export FONTCONFIG_FILE=$NIX_LINK/etc/X11/fonts.conf
      fi

      # source $DOTFIELD_DIR/lib/color.sh
      source ${pkgs.dotfield-config}/zsh/functions.zsh
      source ${pkgs.dotfield-config}/zsh/options.zsh

      ### zsh-notify config
      zstyle ':notify:*' error-title "Command failed"
      zstyle ':notify:*' success-title "Command finished"
      zstyle ':notify:*' error-title "Command failed (in #{time_elapsed} seconds)"
      zstyle ':notify:*' success-title "Command finished (in #{time_elapsed} seconds)"
      zstyle ':notify:*' command-complete-timeout 15
      zstyle ':notify:*' app-name sh
      zstyle ':notify:*' expire-time 2500
      zstyle ':notify:*' blacklist-regex 'find|git'
      zstyle ':notify:*' enable-on-ssh yes
    '';

    sessionVariables = {
      ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
      ZSH_DATA = "${config.xdg.dataHome}/zsh";

      # Needed for xwidgets in emacs
      WEBKIT_FORCE_SANDBOX = 0;
      WEBKIT_DISABLE_COMPOSITING_MODE = 1;
    };

    # Don't source /etc/zprofile and /etc/zshrc
    # This is the end of $ZDOTDIR/.zshenv
    envExtra = ''
      # DEBUG: print an informational message announcing the name of each file loaded
      # set -o SOURCE_TRACE

      # Don't source global RC files
      setopt no_global_rcs
    '';
  };
}
