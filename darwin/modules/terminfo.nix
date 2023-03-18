# Source: nixpkgs@7b06206fa24198912cea58de690aa4943f238fbf

# This module manages the terminfo database
# and its integration in the system.
{ config, lib, pkgs, ... }:

with lib;

{

  options.environment.enableAllTerminfo = with lib;
    mkOption {
      default = false;
      type = types.bool;
      description = lib.mdDoc ''
        Whether to install all terminfo outputs
      '';
    };

  config = {
    # can be generated with: filter (drv: (builtins.tryEval (drv ? terminfo)).value) (attrValues pkgs)
    environment.systemPackages = [ pkgs.ncurses.dev ] ++ (lib.optionals config.environment.enableAllTerminfo
      (map (x: x.terminfo) (with pkgs; [
        alacritty
        mtm
        rxvt-unicode-unwrapped
        rxvt-unicode-unwrapped-emoji
        termite
        wezterm
      ])));

    environment.pathsToLink =
      [ "/share/terminfo" "/Contents/Resources/terminfo" ];

    environment.etc.terminfo = {
      source = "${config.system.path}/share/terminfo";
    };

    environment.extraInit = ''
      # reset TERM with new TERMINFO available (if any)
      export TERM=$TERM
    '';
  };
}
