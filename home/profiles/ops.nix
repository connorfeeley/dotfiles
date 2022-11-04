# It's lonely in here.
{ config
, lib
, pkgs
, ...
}: {
  home.packages = with pkgs; [ nixops ];
}
