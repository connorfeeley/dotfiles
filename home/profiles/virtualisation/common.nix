{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux isDarwin;
in
lib.mkMerge [
  {
    home.packages = with pkgs; [ distrobox ];
  }
]
