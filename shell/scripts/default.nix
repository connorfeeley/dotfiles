{ lib, pkgs, ... }:
let inherit (pkgs) stdenv;
in {
  dotfiles-docs =
    let
      extraPath = lib.makeBinPath (with pkgs; [ pandoc jq curl ]);
      writeProgram = name: env: src:
        pkgs.runCommand name ({
          inherit src;
          inherit (stdenv) shell;
          path = extraPath;
        } // env) ''
          mkdir -p $out/bin
          substitute "$src" "$out/bin/${name}" \
            --subst-var shell \
            --subst-var path
          chmod +x "$out/bin/${name}"
        '';
    in
    writeProgram "dotfiles-docs" { } ./ci/docs.sh;
}
