final: prev: {
  kitty-helpers = final.lib.recurseIntoAttrs (final.callPackage ./kitty-helpers.nix {});

  svlangserver = (final.callPackage ./svlangserver { }).package.override {
    src = builtins.fetchGit {
      url = "https://github.com/imc-trading/svlangserver.git";
      ref = "master";
      rev = "7f53c7f3394447bdd06de9566cd7240aa6cf0c8e";
    };
  };

  svls = final.callPackage ./svls.nix { };

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
