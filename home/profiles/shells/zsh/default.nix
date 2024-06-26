{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv) isDarwin;
  inherit (config.lib) dotfiles;
  configDir = dotfiles.userConfigPath + "/zsh";

  shellAliases = (import ../abbrs.nix) // (import ../aliases.nix) // {
    nix-callpackage = ''(){ nix-build -E "with import <nixpkgs> {}; callPackage ''${1:-./default.nix} {}"; }'';
    nix-callshell = ''(){ nix-shell -E "with import <nixpkgs> {}; callPackage ''${1:-./default.nix} {}"; }'';
  } // (if isDarwin then {
    # Alias 'tailscale' to MAS Tailscale binary
    tailscale = "/Applications/Tailscale.app/Contents/MacOS/Tailscale";

    # Time Machine backup normally runs as a low-profile process; run as high priority instead
    tm_high_prio = "sudo sysctl debug.lowpri_throttle_enabled=0";

    # Reboot after FileVault unlock
    reboot = "sudo fdesetup authrestart -user cfeeley -verbose";
  } else { });

  # Flag to enable zprof
  enableZprof = false;

  # This is marked as unfree, and even though
  github-copilot-cli = pkgs.github-copilot-cli.overrideAttrs (oldAttrs: {
    license.free = true;
  });

  enableGitHubCopilot = false;
in
{
  imports = [ ../common.nix ];

  home.packages = [ pkgs.zsh pkgs.pure-prompt ] ++ lib.optional enableGitHubCopilot github-copilot-cli;

  # Must be disabled for emacs-vterm integration to work.
  # Integration is handled manually in zsh.initExtra.
  programs.starship.enableZshIntegration = false;

  programs.zsh = {
    inherit shellAliases;

    enable = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    # NOTE: conflicts with 'zdharma/fast-syntax-highlighting'
    syntaxHighlighting.enable = false;
    autosuggestion.enable = false;
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

    dirHashes = {
      docs = if isDarwin then "$HOME/Documents" else "$HOME/documents";
      vids = "$HOME/Videos";
      dl = "$HOME/Downloads";
      dev = "$HOME/dev";
      src = "$HOME/source";
      cfg = "$XDG_CONFIG_HOME";
      dots = "$XDG_CONFIG_HOME/dotfiles";
      doom = "$XDG_CONFIG_HOME/doom";
      emacs = "$XDG_CONFIG_HOME/emacs";
    };

    zplug = {
      enable = true;
      plugins = [
        # { name = ""; tags = ""; }

        # WARNING: conflicts with 'programs.zsh.enableSyntaxHighlighting'
        {
          name = "zdharma-continuum/fast-syntax-highlighting";
          tags = [ "defer:2" ];
        }
        {
          name = "zsh-users/zsh-history-substring-search";
          tags = [ "defer:3" ];
        }

        { name = "zsh-users/zsh-autosuggestions"; }
        { name = "agkozak/zsh-z"; }
        { name = "marzocchi/zsh-notify"; }
        { name = "hlissner/zsh-autopair"; }
        { name = "scriptingosx/mac-zsh-completions"; }

        # Pure prompt
        { name = "mafredri/zsh-async"; }
        {
          name = "sindresorhus/pure";
          tags = [ "use:pure.zsh" "as:theme" ];
        }
      ];
    };

    # This is the top of $ZDOTDIR/.zshrc
    initExtraFirst = ''
      ${
        # Optionally enable zprof module; run 'zprof' after shell startup to see profiling results
        lib.optionalString enableZprof "zmodload zsh/zprof"
      }
      if [[ "$TERM" == "dumb" ]]; then
        unsetopt zle
        unsetopt prompt_cr
        unsetopt prompt_subst
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

      ${lib.optionalString isDarwin ''
        # Add homebrew completions to fpath
        # (includes completions for 'brew', although those are also available in
        # /opt/homebrew/completions/zsh)
        fpath=(/opt/homebrew/share/zsh/site-functions $fpath)
      ''}
    '';

    # After plugin init and history init in $ZDOTDIR/.zshrc
    # Followed by zoxide, command-not-found, direnv, GPG, aliases init.
    # Finally, followed by zsh-syntax-highlighting.
    initExtra =
      let
        pure-prompt-config = ''
          # ### Pure
          # autoload -U promptinit; promptinit
          # prompt off
          PURE_PROMPT_SYMBOL="λ"
          # prompt pure
        '';
        github-copilot-cli-config = lib.optionalString enableGitHubCopilot  ''
          ### GitHub Copilot CLI
          eval "$(${github-copilot-cli}/bin/github-copilot-cli alias -- "$0")"
          '';
        zsh-notify-config = ''
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
      in
      ''
        ### Starship
        # eval "$(${pkgs.starship}/bin/starship init zsh)"

        ${pure-prompt-config}

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
          source ${configDir}/vterm.zsh
        fi
        # echo "\$TERM = $TERM	\$INSIDE_EMACS = $INSIDE_EMACS"

        # bind DEL to delete-char  make `vterm-send-delete` delete char (on darwin, at least)
        bindkey "\e[3~" delete-char

        # MacOS only: XQuartz
        if [ "$(uname)" = "Darwin" -a -n "$NIX_LINK" -a -f $NIX_LINK/etc/X11/fonts.conf ]; then
          export FONTCONFIG_FILE=$NIX_LINK/etc/X11/fonts.conf
        fi

        # source $DOTFILES_DIR/lib/color.sh
        source ${configDir + "/functions.zsh"}
        source ${configDir + "/options.zsh"}

        ${github-copilot-cli-config}

        ${zsh-notify-config}
      '';

    sessionVariables = {
      ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
      ZSH_DATA = "${config.xdg.dataHome}/zsh";

      # Needed for xwidgets in emacs
      WEBKIT_FORCE_SANDBOX = 0;
      WEBKIT_DISABLE_COMPOSITING_MODE = 1;

      # zsh-autosuggestions config
      # ZSH_AUTOSUGGEST_STRATEGY = [ "history" "completion" ];

      # Hundredths of a second to wait for escape sequence (?)
      # KEYTIMEOUT = 40;
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
