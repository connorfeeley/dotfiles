{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux;
in
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = with pkgs; [
    sourcetrail # <- Rest in peace sourcetrail, the best C++ exploration tool ever to live.
  ];
}
