{ lib, pkgs, ... }:
let inherit (pkgs) stdenv;
in {
  dotfiles-docs =
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
    writeProgram "dotfiles-docs" { } ./ci/docs.sh;
}
