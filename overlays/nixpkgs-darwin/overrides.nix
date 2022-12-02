_channels: _final: _prev: {
  __dontExport = true;

  ###
  ### Packages from nixpkgs-darwin channel (better chance of hitting the binary cache)
  ###
  # inherit (channels.nixos-stable)
  #   # FIXME(darwin): mesa build fails otherwise.
  #   ffmpeg
  #   youtube-dl
  #   mpv
  #   kitty

  #   # Otherwise, builds nodejs from source. Slow.
  #   starship
  #   js-beautify
  #   ;
}
