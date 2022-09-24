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
in
{
  imports = [
    ./fzf.nix
    ./starship.nix
  ];

  home.packages =
    let
      md = pkgs.writeShellScriptBin "md" ''
        [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1"
      '';

      stderred-wrapper = pkgs.writeShellApplication {
        name = "stderred";
        runtimeInputs = with pkgs; [ stderred ];
        text = ''
          # Export LD_PRELOAD with stderred hooks
          export LD_PRELOAD="${pkgs.stderred}/lib/libstderred.so''${LD_PRELOAD:+:$LD_PRELOAD}"

          # Run command
          "$@"
        '';
      };
      fnix = pkgs.writeShellApplication {
        name = "fnix";
        runtimeInputs = with pkgs; [ nix nix-json-progress ];
        text = ''
          nix --log-format internal-json "$@" |& ${pkgs.nix-json-progress}/bin/nix-json-progress
        '';
      };
    in
    [
      md
      stderred-wrapper # Highlight stderr in red
      fnix
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
