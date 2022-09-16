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
      git-format =
        let
          src = pkgs.fetchFromGitHub {
            owner = "kimgr";
            repo = "git-format";
            rev = "8c7e32d74fdd8a3498390846f22f1baa953bbeb2";
            sha256 = "sha256-bJ+4Mz5JfMm8V4UWhkkJM/xBFTnfpbiZLOwkAYAabjg=";
          };
        in
        pkgs.writeShellApplication {
          name = "git-format";
          runtimeInputs = with pkgs; [ python3 ];
          text = "${src}/git-format";
        };
    in
    [
      md
      rsc
      stderred-wrapper # Highlight stderr in red
      fnix
      git-format
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
