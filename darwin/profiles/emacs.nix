{ ...
}: {
  environment.variables = {
    DOOMDIR = "$XDG_CONFIG_HOME/doom";
    EMACSDIR = "$XDG_CONFIG_HOME/emacs";
  };

  homebrew = {
    taps = [{ name = "railwaycat/emacsmacport"; }];
    brews = [
      {
        name = "emacs-mac";
        args = [
          "with-natural-title-bar"
          "with-starter"
          # "with-mac-metal"
          "with-native-compilation"
          "with-xwidgets"
        ];
      }
      # :lang org (macOS only)
      { name = "pngpaste"; }
      { name = "coreutils"; }
    ];
  };
}
