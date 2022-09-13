channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-stable)
    zathura
    wally-cli
    ;

  inherit (channels.nixos-stable-21-11)
    sourcetrail
    ;
}
