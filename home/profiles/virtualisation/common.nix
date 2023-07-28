{ lib, pkgs, ... }:

{
  home.packages = with pkgs;
    (lib.optionals pkgs.stdenv.isLinux [ distrobox ]) ++
    [ arion ];
}
