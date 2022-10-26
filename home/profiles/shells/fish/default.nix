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

  programs.starship.enableFishIntegration = true;

  programs.fish = {
    inherit
      # FIXME: watch out, some of these abbrs may have undesirable results when
      # expanded inline. needs review.
      shellAbbrs
      shellAliases
      ;

    enable = true;
    autopair.enable = true;
    fifc.enable = true;

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
        body = ''vterm_cmd find-file "$(realpath "''${@:-.}")'';
      };
    };

    interactiveShellInit = ''
      # "Required" by `fifc`
      # set -Ux fifc_editor $EDITOR

      [[ -n $EMACS_VTERM_PATH ]] && [[ -f $EMACS_VTERM_PATH/etc/emacs-vterm-zsh.sh ]] && source $EMACS_VTERM_PATH/etc/emacs-vterm.fish
    '';
  };
}
