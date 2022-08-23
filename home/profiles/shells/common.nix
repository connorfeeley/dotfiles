{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib.dotfield.whoami) githubUserName;

  envInit = import ./env-init.sh.nix;

  shellAliases =
    (import ./abbrs.nix)
    // (import ./aliases.nix);

  fdBin = "${pkgs.fd}/bin/fd";
in {
  imports = [
    ./fzf.nix
    ./starship.nix
  ];

  home.packages = [
    (pkgs.writeShellScriptBin "md" ''
      [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1"
    '')
  ];

  programs.bash = {
    inherit shellAliases;

    enable = true;
    bashrcExtra = envInit;
    # profileExtra = "";

    sessionVariables = {
      BASH_COMPLETION_USER_FILE = "${config.xdg.dataHome}/bash/completion";
    };
  };
}
