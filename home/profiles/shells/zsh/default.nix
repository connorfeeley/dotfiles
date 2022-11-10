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
    enableSyntaxHighlighting = true;
    enableAutosuggestions = false;
    enableVteIntegration = false;

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

    plugins = [
      {
        name = "fast-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zdharma-continuum";
          repo = "fast-syntax-highlighting";
          rev = "v1.55";
          sha256 = "0h7f27gz586xxw7cc0wyiv3bx0x3qih2wwh05ad85bh2h834ar8d";
        };
      }
      {
        name = "zsh-completions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-completions";
          rev = "0.34.0";
          sha256 = "0jjgvzj3v31yibjmq50s80s3sqi4d91yin45pvn3fpnihcrinam9";
        };
      }
      {
        name = "zsh-history-substring-search";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-history-substring-search";
          rev = "v1.0.2";
          sha256 = "0y8va5kc2ram38hbk2cibkk64ffrabfv1sh4xm7pjspsba9n5p1y";
        };
      }
      {
        name = "zsh-autopair";
        src = pkgs.fetchFromGitHub {
          owner = "hlissner";
          repo = "zsh-autopair";
          rev = "v1.0";
          sha256 = "1h0vm2dgrmb8i2pvsgis3lshc5b0ad846836m62y8h3rdb3zmpy1";
        };
      }
      {
        name = "zsh-z";
        src = pkgs.fetchFromGitHub {
          owner = "agkozak";
          repo = "zsh-z";
          rev = "aaafebcd97424c570ee247e2aeb3da30444299cd";
          sha256 = "147rwiqn5xs0vx7pkqvl1480s7fv7f5879cq6k42pn74jawzhspm";
        };
      }
      {
        name = "mac-zsh-completions";
        src = pkgs.fetchFromGitHub {
          owner = "scriptingosx";
          repo = "mac-zsh-completions";
          rev = "303f25c8b30f3c7351a7bcaaf4b6f01818c3f1ad";
          sha256 = "sha256-06mEWuZsfTLNKodqHGTxiakZf0MvWsvoSvnt2IW/Nkk=";
        };
      }
      {
        name = "zsh-notify";
        src = pkgs.fetchFromGitHub {
          owner = "marzocchi";
          repo = "zsh-notify";
          rev = "4d43b452c1a30ad1c854eb72a1fafb9b73d871ab";
          sha256 = "sha256-yVReDAcPVnwGo8dpjH1fSghQPdCYCIGfkUDD/WLW73k=";
        };
      }
    ];

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

    initExtraBeforeCompInit = ''
    '';

    initExtra = ''
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
    '';

    sessionVariables = {
      ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
      ZSH_DATA = "${config.xdg.dataHome}/zsh";

      # Needed for xwidgets in emacs
      WEBKIT_FORCE_SANDBOX = 0;
      WEBKIT_DISABLE_COMPOSITING_MODE = 1;
    };

    # Don't source /etc/zprofile and /etc/zshrc
    envExtra = ''
      setopt no_global_rcs
    '';
  };
}
