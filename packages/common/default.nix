final: _prev: {
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

  xsct = final.callPackage ./xsct.nix { };

  xantfarm = final.callPackage ./xantfarm.nix { };

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
