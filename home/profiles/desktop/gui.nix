moduleArgs @ { config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in
{
  home.packages = with pkgs; [
    element-desktop # Matrix client
  ];
}
