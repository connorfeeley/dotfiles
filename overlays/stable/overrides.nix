channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    fish
    iosevka-bin
    nerdfonts
    python3Packages
    statix
    ;

  inherit
    (channels.nixos-unstable.nodePackages)
    pyright
    ;

  ripgrep = final.ripgrep.override { withPCRE2 = true; };

  lshw = final.lshw.override { withGUI = true; };

  # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
  httpie = final.python3Packages.httpie;
}
