channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    fish
    iosevka-bin
    nerdfonts
    statix
    ;

  inherit
    (channels.nixos-unstable.nodePackages)
    pyright
    ;
}
