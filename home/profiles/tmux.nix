{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux;
in
{
  programs.tmux = {
    enable = true;
    agressiveResize = true;
    clock24 = true;
    keyMode = true;

    plugins = with pkgs; [
      # tmuxPlugins.cpu
      # {
      #   plugin = tmuxPlugins.resurrect;
      #   extraConfig = "set -g @resurrect-strategy-nvim 'session'";
      # }
      # {
      #   plugin = tmuxPlugins.continuum;
      #   extraConfig = ''
      #     set -g @continuum-restore 'on'
      #     set -g @continuum-save-interval '60' # minutes
      #   '';
      # }
    ];
    secureSocket = true;

    # Run the sensible plugin at the top of the configuration.
    sensibleOnTop = true;

    tmuxinator.enable = true;
  };
}
