channels: final: prev: {
  __dontExport = true;

  inherit (channels.nixpkgs-darwin-stable)
    ffmpeg
    youtube-dl
    mpv
    kitty
    wally-cli
    ;

  inherit (channels.nixos-stable)
    ;

  inherit (channels.nixos-unstable)
    ;
}
