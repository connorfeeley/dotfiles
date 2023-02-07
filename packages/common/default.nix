final: prev: {
  kitty-helpers = final.lib.recurseIntoAttrs (final.callPackage ./kitty-helpers.nix { });

  svlangserver-unwrapped = (final.callPackage ./svlangserver { }).package.override {
    src = builtins.fetchGit {
      url = "https://github.com/imc-trading/svlangserver.git";
      ref = "master";
      rev = "7f53c7f3394447bdd06de9566cd7240aa6cf0c8e";
    };
    nodejs = final.nodejs-14_x;
  };

  svlangserver =
    final.writeShellApplication {
      name = "svlangserver";
      text = with final; "exec ${svlangserver-unwrapped.nodejs}/bin/node ${svlangserver-unwrapped}/lib/node_modules/@imc-trading/svlangserver/bin/main.js";
      runtimeInputs = with final; [ nodejs-14_x svlangserver-unwrapped ];
    };

  svls-local = throw "use svls from nixpkgs; this is the flake-local derivation";
  svlint-local = throw "use svlint from nixpkgs; this is the flake-local derivation";

  emacs28Macport = final.darwin.apple_sdk_11_0.callPackage ./emacs-macport/macport.nix {
    withMacport = true;
    gconf = null;
    inherit (final.darwin.apple_sdk_11_0.frameworks)
      AppKit Carbon Cocoa IOKit OSAKit Quartz QuartzCore WebKit Security UniformTypeIdentifiers
      ImageCaptureCore GSS ImageIO;
    inherit (final.darwin) sigtool;
    inherit (final) gmp;
  };

  appmenu-gtk3-module = final.callPackage ./appmenu-gtk3-module.nix { };

  fildem-global-menu = final.callPackage ./fildem-global-menu.nix { inherit (final) lib stdenv fetchFromGitHub; };

  xsct = final.callPackage ./xsct.nix { };

  xantfarm = final.callPackage ./xantfarm.nix { };

  mdio-tools = final.callPackage ./mdio-tools.nix { };

  dotfield-sync = prev.callPackage ./flake-scripts/dotfield-sync-repos.nix {
    name = "dotfield-sync";
    inherit (prev.nodePackages) git-run;
  };

  dotfield-push = prev.callPackage ./flake-scripts/dotfield-push-repos.nix {
    name = "dotfield-push";
    inherit (prev.nodePackages) git-run;
  };

  dotfield-rebuild =
    let
      extraPath = prev.lib.makeBinPath [ prev.nix-output-monitor ];
      writeProgram = name: env: src:
        prev.substituteAll ({
          inherit name src;
          inherit (prev.stdenv) shell;
          path = "${extraPath}";
          dir = "bin";
          isExecutable = true;
        } // env);
    in
    writeProgram "dotfield-rebuild"
      {
        nom = "${prev.nix-output-monitor}/bin/nom";
      } ./flake-scripts/dotfield-rebuild.sh;

  dotfield-doom =
    let
      extraPath = prev.lib.makeBinPath [ ];
      writeProgram = name: env: src:
        prev.substituteAll ({
          inherit name src;
          inherit (prev.stdenv) shell;
          path = "${extraPath}";
          dir = "bin";
          isExecutable = true;
        } // env);
    in
    writeProgram "dotfield-doom" { } ./flake-scripts/doom-rebuild.sh;

  nixos-rebuild-remote = prev.callPackage ./flake-scripts/nixos-rebuild-remote.nix {
    name = "nixos-rebuild-remote";
    inherit (prev) writeShellApplication nixos-rebuild;
  };

  # openssh =
  #   let
  #     # Fixed ssh-copy-id for old+new dropbear compatibility
  #     ssh-copy-id =
  #       final.stdenv.mkDerivation {
  #         name = "ssh-copy-id";
  #         src = ./ssh-copy-id;
  #         installPhase = ''
  #           mkdir -p $out/bin
  #           cp ssh-copy-id $out/bin/ssh-copy-id
  #           chmod +x $out/bin/ssh-copy-id
  #         '';
  #       };
  #   in
  #   final.symlinkJoin {
  #     name = "openssh";
  #     paths = [ prev.openssh ssh-copy-id ];
  #     buildInputs = [ final.makeWrapper ];
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
