{
  config,
  lib,
  pkgs,
  ...
}: let
  shellAliases =
    (import ../abbrs.nix)
    // (import ../aliases.nix);
in {
  imports = [../common.nix];

  home.packages = with pkgs; [
    zsh
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

    plugins = [
      {
        # will source zsh-autosuggestions.plugin.zsh
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "junegunn";
          repo = "fzf";
          rev = "0.32.1";
          sha256 = "194xz7jrjsl9i88gy9kr90sw9aif9iskwdnhbq85l48h86hlrm42";
        };
      }
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
        # will source zsh-autosuggestions.plugin.zsh
        name = "zlong_alert";
        src = pkgs.fetchFromGitHub {
          owner = "kevinywlui";
          repo = "zlong_alert.zsh";
          rev = "f651ba8fff4ed7326354c2d85d29367fecdc6c2e";
          sha256 = "sha256-9LhGuv6yJ+XOj8t40ejXhqlcjNYUdyla6f4sVb/Q+b0=";
        };
      }
    ];

    initExtraFirst = ''
      # When TERM=dumb (such as when using TRAMP):
      # - Unset problematic options
      # - Use a dead-simple prompt
      # - Don't source rest of zshrc (return early)
      if [[ "$TERM" == "dumb" ]]; then
          unset zle_bracketed_paste
          unset zle
          PS1='$ '
          return
      fi
    '';

    initExtraBeforeCompInit = ''
    '';

    initExtra = ''
      source $DOTFIELD_DIR/lib/color.sh
      source ${pkgs.dotfield-config}/zsh/functions.zsh
      source ${pkgs.dotfield-config}/zsh/options.zsh

      eval "$(${pkgs.starship}/bin/starship init zsh)"

      # Use vi-mode when:
      # - TERM is not dumb (which it is over TRAMP)
      # AND:
      # - We are not in emacs
      # - We are not in an emacs vterm over TRAMP
      if [[ $TERM != "dumb" && ( -z $INSIDE_EMACS || "''${INSIDE_EMACS/*tramp*/tramp}" != "tramp") ]]; then
        # Vi keybindings
        source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
      else
        source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh

        # Options we don't want in vterm
        unset zle_bracketed_paste
        unset zle

        # Emacs keybindings
        bindkey -e

        # # Source vterm-specific configuration
        source $DOTFIELD_DIR/config/emacs/vterm.zsh
      fi
      # echo "\$TERM = $TERM	\$INSIDE_EMACS = $INSIDE_EMACS"

      # bind  DEL to delete-char  make `vterm-send-delete` delete char
      bindkey "\e[3~" delete-char

      # MacOS only: XQuartz
      if [ "$(uname)" = "Darwin" -a -n "$NIX_LINK" -a -f $NIX_LINK/etc/X11/fonts.conf ]; then
        export FONTCONFIG_FILE=$NIX_LINK/etc/X11/fonts.conf
      fi
    '';

    sessionVariables = {
      ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
      ZSH_DATA = "${config.xdg.dataHome}/zsh";
    };

    # Don't source /etc/zprofile and /etc/zshrc
    envExtra = ''
      setopt no_global_rcs
    '';
  };
}
