{ pkgs, callPackage, nodePackages, ... }:
let
  nixFlags = [
    "--verbose"
    "--show-trace"
    "--print-build-logs"
  ];
  ### Nix 'aliases' (defined as executables so as to be usable from Emacs)
  # 'nix'
  n = pkgs.writeShellScriptBin "n" ''
    exec ${pkgs.nix}/bin/nix ${builtins.concatStringsSep " " nixFlags} "$@"
  '';
  # 'nix build'
  nb = pkgs.writeShellScriptBin "nb" ''
    exec ${pkgs.nix}/bin/nix build ${builtins.concatStringsSep " " nixFlags} "$@"
  '';
in
{
  # Nix 'aliases'
  inherit n nb;

  xsct = callPackage ./xsct.nix { };

  xantfarm = callPackage ./xantfarm.nix { };

  mdio-tools = callPackage ./mdio-tools.nix { };

  nixos-rebuild-remote =
    callPackage ./flake-scripts/nixos-rebuild-remote.nix {
      name = "nixos-rebuild-remote";
      inherit (pkgs) writeShellApplication nixos-rebuild;
    };

  ediff-tool = pkgs.stdenv.mkDerivation {
    name = "ediff-tool";
    src = ./ediff-tool;
    installPhase = ''
      mkdir -p $out/bin
      cp bin/* $out/bin/
    '';
  };

  # openssh =
  #   let
  #     # Fixed ssh-copy-id for old+new dropbear compatibility
  #     ssh-copy-id =
  #       pkgs.stdenv.mkDerivation {
  #         name = "ssh-copy-id";
  #         src = ./ssh-copy-id;
  #         installPhase = ''
  #           mkdir -p $out/bin
  #           cp ssh-copy-id $out/bin/ssh-copy-id
  #           chmod +x $out/bin/ssh-copy-id
  #         '';
  #       };
  #   in
  #   pkgs.symlinkJoin {
  #     name = "openssh";
  #     paths = [ pkgs.openssh ssh-copy-id ];
  #     buildInputs = [ pkgs.makeWrapper ];
  #   };

  ##: third-party scripts ------------------------------------------------------

  # darwin-rebuild =
  # let
  #   extraPath = lib.makeBinPath [ config.nix.package pkgs.coreutils pkgs.jq pkgs.git ];
  #   writeProgram = name: env: src:
  #     pkgs.substituteAll ({
  #       inherit name src;
  #       dir = "bin";
  #       isExecutable = true;
  #     } // env);
  # in writeProgram "darwin-rebuild"
  #   {
  #     inherit (config.system) profile;
  #     inherit (stdenv) shell;
  #     path = "${extraPath}:${config.environment.systemPath}";
  #   }
  #   ./darwin-rebuild.sh;
}
