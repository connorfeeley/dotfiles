final: prev: {
  kitty-helpers = final.lib.recurseIntoAttrs (final.callPackage ./kitty-helpers.nix {});

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

  emacs28Macport = final.callPackage ./emacs-macport/macport.nix {
    withMacport = true;
    gconf = null;
    inherit (final.darwin.apple_sdk.frameworks)
      AppKit Carbon Cocoa IOKit OSAKit Quartz QuartzCore WebKit Security UniformTypeIdentifiers
      ImageCaptureCore GSS ImageIO;
    inherit (final.darwin) sigtool;
  };

  ##: dotfield internals -------------------------------------------------------

  dotfield-config = final.stdenv.mkDerivation {
    name = "dotfield-config";
    # FIXME: what the...
    src = final.gitignoreSource ../../home/users/cfeeley/config;
    installPhase = ''
      mkdir -p $out
      cp -R * $out/
    '';
  };

  ##: third-party scripts ------------------------------------------------------
}
