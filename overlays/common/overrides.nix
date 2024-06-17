channels: final: prev: {
  __dontExport = true;

  ripgrep = prev.ripgrep.override { withPCRE2 = true; };

  xmonad = prev.xmonad-config;

  lshw = prev.lshw.override { withGUI = true; };

  # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
  httpie = prev.python3Packages.httpie;

  pythonPackagesExtensions = prev.pythonPackagesExtensions
    ++ [ (python-final: python-prev: { }) ];

  inherit (prev) installApplication;

  # Fix nixos-option flake support
  nixos-option = prev.symlinkJoin {
    name = "nixos-option";
    paths = [ prev.nixos-option ];
    buildInputs = [ prev.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/nixos-option --add-flags '-I nixpkgs="$DOTFILES_DIR/lib/compat"'
    '';
  };
}
