channels: final: prev: {
  __dontExport = true;

  ###
  ### Packages from darwin-stable channel (better chance of hitting the binary cache)
  ###
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

  ###
  ### Packages from nixos-unstable channel (sometimes also a better chance of hitting the binary cache)
  ###
  inherit (channels.nixos-unstable)
    ;

  ###
  ### Packages from nixos-stable channel
  ###
  inherit (channels.nixos-stable)
    ;

}
