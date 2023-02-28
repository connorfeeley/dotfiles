{ lib, pkgs, ... }:
let inherit (pkgs) stdenv nix-output-monitor;
in {
  dotfield-docs =
    let
      extraPath = lib.makeBinPath (with pkgs; [ pandoc jq curl ]);
      writeProgram = name: env: src:
        pkgs.substituteAll ({
          inherit name src;
          inherit (stdenv) shell;
          path = "${extraPath}";
          dir = "bin";
          isExecutable = true;
        } // env);
    in
    writeProgram "dotfield-docs" { } ./ci/docs.sh;
}
