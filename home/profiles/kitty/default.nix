{ config, lib, pkgs, inputs, ... }:
let
  inherit (inputs) base16-kitty nix-colors;
  inherit (pkgs.stdenv.hostPlatform) isDarwin isAarch64;
  inherit (config.lib.dotfield.features) hasTwm hasPragPro;

  socket = "unix:/tmp/kitty-socket";

  settings = import ./settings.nix { inherit lib hasTwm socket; };
  modus-vivendi-faint = import ./modus-vivendi-faint.nix;

  # via home-manager kitty module
  toKittyConfig = lib.generators.toKeyValue {
    mkKeyValue = key: value:
      let
        value' =
          if lib.isBool value then
            (lib.hm.booleans.yesNo value)
          else
            builtins.toString value;
      in
      "${key} ${value'}";
  };

  mkTheme = name: import ./colors.nix nix-colors.colorSchemes.${name};
  mkTheme' = name: toKittyConfig (mkTheme name);
  mkThemeBuiltin = name:
    pkgs.kitty-themes.outPath + "/themes/" + name + ".conf";

  mkFontFeatures = name: features:
    "font_features ${name} ${lib.concatStringsSep " " features}";

  mkFontFeatures' = family: styles: features:
    lib.concatMapStringsSep "\n"
      (style: mkFontFeatures "${family}-${style}" features)
      styles;

  pragmataProExtras =
    let
      fontStyles = [ "Regular" "Italic" "BoldItalic" ];
      # https://fsd.it/pragmatapro/Handbook.png
      fontFeatures = [
        "+calt" # Standard programming ligatures.
        # "+frac" # Fraction stylization.
        # "+ss12" # Assorted iconizations.
        # "+ss13" # Smooth graphs e.g. for git log.
      ];
    in
    ''
      ${mkFontFeatures' "PragmataProMono" fontStyles fontFeatures}
    '';
  # FIXME: reduce the amount of merging -> reduce complecity
in
lib.mkMerge [
  (lib.mkIf isDarwin {
    # Handled by the Homebrew module
    # This populates a dummy package to satisfy the requirement
    programs.kitty.package = pkgs.runCommand "kitty-0.0.0" { } "mkdir $out";

    programs.kitty.darwinLaunchOptions =
      [ "--single-instance" "--listen-on=${socket}" ];
  })

  {
    home.sessionVariables = {
      KITTY_CONFIG_DIRECTORY = "${config.xdg.configHome}/kitty";
      # FIXME: necessary?
      KITTY_SOCKET = socket;
    };

    programs.kitty = {
      enable = true;
      settings = settings // {
        font_size = if (isDarwin && isAarch64) then "12" else "16";
        confirm_os_window_close = "0";
        # if (isDarwin)
        # then "1"
        # else "0";
        # 85% opacity
        background_opacity = if isDarwin then "0.85" else "1.0";
      };
      # // (lib.mkForce modus-vivendi-faint); # force kitty colorscheme (also set by stylix)
      keybindings = {
        # kitty_mod: ctrl+shift, or ⌘ (cmd) key on macos
        "kitty_mod+n" = "new_os_window_with_cwd";
        "ctrl+alt+enter" = "launch --cwd=current --location=neighbor";
        # Default: create new split
        # "kitty_mod+enter" = "new_window_with_cwd";

        # Vim-style window navigation
        "kitty_mod+left" = "neighboring_window left";
        "kitty_mod+right" = "neighboring_window right";
        "kitty_mod+down" = "neighboring_window down";
        "kitty_mod+up" = "neighboring_window up";

        # Change layouts
        "kitty_mod+l" = "next_layout";
      };
      font = {
        name = lib.mkForce "Iosevka Term";
        size = 10;
      };
      extraConfig = ''
        ${lib.optionalString hasPragPro pragmataProExtras}

        # Include theme - symlink to either the selected dark or light theme
        include ${config.xdg.configHome}/kitty/current-theme.conf
      '';
    };
  }
  (lib.mkIf isDarwin {
    programs.kitty.keybindings = {
      # Tried unbinding ⌘+w from close tab, but it doesn't seem to be taking.
      # Enabled confirm_os_window_close on darwin as a stop-gap.
      "ctrl+shift+w" = "no_op";
      "kitty_mod+w" = "no_op";
    };
  })
  {
    xdg.configFile =
      let
        chosenTheme = dark;
        dark = mkThemeBuiltin "Doom_One";
        light = mkThemeBuiltin "Doom_One_Light";
      in
      {
        ###
        ### Theming
        ###
        "kitty/base16-kitty".source = base16-kitty.outPath;
        "kitty/nix-kitty-themes".source = pkgs.kitty-themes.outPath;
        "kitty/themes/dark.conf".source = dark;
        "kitty/themes/light.conf".source = light;
        "kitty/current-theme.conf".source = chosenTheme;

        "kitty/session".text = ''
          # Start new sessions in the previous working directory
          # https://sw.kovidgoyal.net/kitty/overview/#startup-sessions
          # https://sw.kovidgoyal.net/kitty/faq/#how-do-i-open-a-new-window-or-tab-with-the-same-working-directory-as-the-current-window
          launch --cwd=current
        '';
      };
  }
]
