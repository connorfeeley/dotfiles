{ lib
, pkgs
, ...
}:
let inherit (pkgs.stdenv.hostPlatform) isLinux isAarch64;
in lib.mkIf (isLinux && !isAarch64) {
  home.packages = with pkgs; [
    spotify
    spotify-tui
  ];
}
