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
  dotfield-sync = pkgs.callPackage ./ci/dotfield-sync-repos.nix {
    inherit (pkgs.nodePackages) git-run;
  };

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

  dotfield-doom =
    let
      extraPath = lib.makeBinPath [ ];
      writeProgram = name: env: src:
        pkgs.substituteAll ({
          inherit name src;
          inherit (stdenv) shell;
          path = "${extraPath}";
          dir = "bin";
          isExecutable = true;
        } // env);
    in
    writeProgram "dotfield-doom" { } ./ci/doom-rebuild.sh;
}
