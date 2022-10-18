{
  config,
  lib,
  pkgs,
  ...
}:
lib.mkIf pkgs.stdenv.isDarwin {
  targets.darwin.search = "Google";
}
