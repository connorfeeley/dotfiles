{ config
, lib
, pkgs
, ...
}:
let
  inherit (builtins) map;
  inherit (config.lib.fish) mkPlugin;

  shellAbbrs = import ../abbrs.nix;
  shellAliases = import ../aliases.nix;
in
{
  imports = [ ../common.nix ];

  home.packages = with pkgs; [
    fishPlugins.done
    fishPlugins.forgit
  ];

  programs.starship.enableFishIntegration = false;

  programs.fish = {
    inherit
      # FIXME: watch out, some of these abbrs may have undesirable results when
      # expanded inline. needs review.
      shellAbbrs
      shellAliases
      ;

    enable = true;
    autopair.enable = true;
    fifc.enable = false;

    plugins = map mkPlugin [
      "replay" #        <- run bash commands replaying changes in fish
      # "bass" #        <- use utilities written for bash in fish
      # "foreign-env" # <- import environment variables exported/modified in bash
      # "babelfish" #   <- translate bash scripts to fish
    ];

    functions = {
      __fish_command_not_found_handler = {
        body = "__fish_default_command_not_found_handler $argv[1]";
        onEvent = "fish_command_not_found";
      };
      vim = {
        description = "Open a file in emacs (from vterm)";
        body = ''
          vterm_cmd find-file $argv
        '';
      };
      fish_prompt = {
        description = "'Pythonista' prompt";
        body = ''
          ###
          ### Prompt
          ###
          if not set -q VIRTUAL_ENV_DISABLE_PROMPT
              set -g VIRTUAL_ENV_DISABLE_PROMPT true
          end
          set_color yellow
          printf '%s' $USER
          set_color normal
          printf ' at '

          set_color magenta
          echo -n (prompt_hostname)
          set_color normal
          printf ' in '

          set_color $fish_color_cwd
          printf '%s' (prompt_pwd)
          set_color normal

          # Line 2
          echo
          if test -n "$VIRTUAL_ENV"
              printf "(%s) " (set_color blue)(basename $VIRTUAL_ENV)(set_color normal)
          end
          printf '↪ '
          set_color normal
        '';
      };
    };

    interactiveShellInit = ''
      # "Required" by `fifc`
      # set -Ux fifc_editor $EDITOR

      source ${pkgs.emacsPackages.vterm}/share/emacs/site-lisp/elpa/vterm-*/etc/emacs-vterm.fish
    '';
  };
}
