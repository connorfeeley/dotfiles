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

  ripgrep = prev.ripgrep.override { withPCRE2 = true; };

  # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
  httpie = final.python3Packages.httpie;
}
