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
    zsh
  ];

  programs.zsh = {
    inherit
      envExtra
      shellAliases
      ;

    enable = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableAutosuggestions = true;

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
        name = "zsh-vi-mode";
        src = pkgs.fetchFromGitHub {
          owner = "jeffreytse";
          repo = "zsh-vi-mode";
          rev = "v0.8.5";
          sha256 = "1wgkqy89qp1kkg64brm42rx3apsfhmpada57vci0lwib3lg2mrhh";
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
      # if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
      #   source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      # fi
    '';

    initExtraBeforeCompInit = ''
      # # Init completion manually
      # autoload compinit; compinit -u
    '';

    initExtra = ''
      source $DOTFIELD_DIR/lib/color.sh
      source ${pkgs.dotfield-config}/zsh/functions.zsh
      source ${pkgs.dotfield-config}/zsh/options.zsh
      source $DOTFIELD_DIR/config/emacs/vterm.zsh

      if [[ $TERM != "dumb" && ( -z $INSIDE_EMACS || $INSIDE_EMACS == "vterm,tramp*" ) ]]; then
        eval "$(${pkgs.starship}/bin/starship init zsh)"
      fi
    '';

    sessionVariables = {
      ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
      ZSH_DATA = "${config.xdg.dataHome}/zsh";
    };
  };
}
