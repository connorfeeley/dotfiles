{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib.dotfield.whoami) githubUserName;
  inherit (pkgs.stdenv) isLinux;

  envInit = import ./env-init.sh.nix;

  shellAliases =
    (import ./abbrs.nix)
    // (import ./aliases.nix);

  fdBin = "${pkgs.fd}/bin/fd";

  rsc = pkgs.symlinkJoin {
    name = "rsc";
    paths = [ pkgs.rsync ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      makeWrapper ${pkgs.rsync}/bin/rsync $out/bin/rsc --add-flags "-rav --progress"
    '';
  };
in
{
  imports = [
    ./fzf.nix
    ./starship.nix
  ];

  home.packages = [
    (pkgs.writeShellScriptBin "md" ''
      [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1"
    '')

    rsc

    # Highlight stderr in red
    pkgs.stderred

    pkgs.nix-json-progress
    (pkgs.writeShellApplication {
      name = "fnix";
      runtimeInputs = with pkgs; [ nix nix-json-progress ];
      text = ''
        nix --log-format internal-json "$@" |& ${pkgs.nix-json-progress}/bin/nix-json-progress
      '';
    })
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
