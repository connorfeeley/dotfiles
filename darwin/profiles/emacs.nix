{ ...
}: {
  environment.variables = {
    DOOMDIR = "$XDG_CONFIG_HOME/doom";
    EMACSDIR = "$XDG_CONFIG_HOME/emacs";
  };

  homebrew = {
    taps = [ { name = "railwaycat/emacsmacport"; } ];
    casks = [ { name = "emacs-mac"; } ];
    brews = [
      # :lang org (macOS only)
      { name = "pngpaste"; }
      { name = "coreutils"; }
    ];
  };
}
