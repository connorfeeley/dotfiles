{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs)
    stdenv
    nix-output-monitor
    ;
in
{
  dotfield-rebuild =
    let
      extraPath = lib.makeBinPath [ nix-output-monitor ];
      writeProgram = name: env: src:
        pkgs.substituteAll ({
          inherit name src;
          inherit (stdenv) shell;
          path = "${extraPath}";
          dir = "bin";
          isExecutable = true;
        } // env);
    in
    writeProgram "dotfield-rebuild"
      {
        nom = "${nix-output-monitor}/bin/nom";
      } ./ci/dotfield-rebuild.sh;
}
