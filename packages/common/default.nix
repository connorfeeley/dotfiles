{pkgs, callPackage, nodePackages, ...}:
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

  svlangserver-unwrapped =
    (callPackage ./svlangserver { }).package.override {
      src = builtins.fetchGit {
        url = "https://github.com/imc-trading/svlangserver.git";
        ref = "master";
        rev = "7f53c7f3394447bdd06de9566cd7240aa6cf0c8e";
      };
      nodejs = pkgs.nodejs-14_x;
    };

  svlangserver = pkgs.writeShellApplication {
    name = "svlangserver";
    text = with pkgs;
      "exec ${svlangserver-unwrapped.nodejs}/bin/node ${svlangserver-unwrapped}/lib/node_modules/@imc-trading/svlangserver/bin/main.js";
    runtimeInputs = with pkgs; [ nodejs-14_x svlangserver-unwrapped ];
  };

  svls-local =
    throw "use svls from nixpkgs; this is the flake-local derivation";
  svlint-local =
    throw "use svlint from nixpkgs; this is the flake-local derivation";

  xsct = callPackage ./xsct.nix { };

  xantfarm = callPackage ./xantfarm.nix { };

  mdio-tools = callPackage ./mdio-tools.nix { };

  dotfield-sync = callPackage ./flake-scripts/dotfield-sync-repos.nix { };

  dotfield-push = callPackage ./flake-scripts/dotfield-push-repos.nix { };

  dotfield-rebuild =
    let
      extraPath = pkgs.lib.makeBinPath [ pkgs.nix-output-monitor ];
      writeProgram = name: env: src:
        pkgs.substituteAll ({
          inherit name src;
          inherit (pkgs.stdenv) shell;
          path = "${extraPath}";
          dir = "bin";
          isExecutable = true;
        } // env);
    in
    writeProgram "dotfield-rebuild"
      {
        nom = "${pkgs.nix-output-monitor}/bin/nom";
      } ./flake-scripts/dotfield-rebuild.sh;

  dotfield-doom =
    let
      extraPath = pkgs.lib.makeBinPath [ ];
      writeProgram = name: env: src:
        pkgs.substituteAll ({
          inherit name src;
          inherit (pkgs.stdenv) shell;
          path = "${extraPath}";
          dir = "bin";
          isExecutable = true;
        } // env);
    in
    writeProgram "dotfield-doom" { } ./flake-scripts/doom-rebuild.sh;

  nixos-rebuild-remote =
    callPackage ./flake-scripts/nixos-rebuild-remote.nix {
      name = "nixos-rebuild-remote";
      inherit (pkgs) writeShellApplication nixos-rebuild;
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
