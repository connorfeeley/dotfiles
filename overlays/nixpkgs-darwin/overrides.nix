channels: final: prev: {
  __dontExport = true;

  inherit (channels.nixpkgs-darwin-stable)
    wally-cli
    ncdu

    # FIXME(darwin): mesa build fails otherwise.
    ffmpeg
    youtube-dl
    mpv
    kitty

    # Otherwise, builds nodejs from source. Slow.
    starship
    js-beautify
    ;

  inherit (channels.nixos-stable)
    ;

  inherit (channels.nixos-unstable)
    ;
}
