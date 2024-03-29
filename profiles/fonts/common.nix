{ inputs', lib, pkgs, ... }:
let inherit (pkgs.stdenv.hostPlatform) isLinux isMacOS;
in {
  environment.systemPackages = lib.optionals isLinux [ pkgs.font-manager ];

  fonts =
    if isLinux then {
      fontDir.enable = true;
      packages = with pkgs;
        [
          b612
          barlow
          emacs-all-the-icons-fonts
          # symbola
          fira
          ibm-plex
          inter
          jost
          public-sans
          julia-mono
          merriweather
          alegreya

          # noto-fonts
          # noto-fonts-cjk
          # noto-fonts-emoji

          # inputs'.ttc-subway-font.packages.ttc-subway
          # inputs'.ttc-subway-font.packages.bloor-yonge-font

          (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })

          font-awesome

          fira-code
          fira-code-symbols
          open-sans
          jetbrains-mono
          siji

          iosevka-bin
        ] ++ (lib.optionals isLinux [
          corefonts
          inconsolata
          liberation_ttf
          dejavu_fonts
          bakoma_ttf
          gentium
          ubuntu_font_family
          terminus_font
        ]) ++ (lib.optionals isMacOS [ ]);
    }
    else {
      fontDir.enable = true;
      fonts = with pkgs;
        [
          b612
          barlow
          emacs-all-the-icons-fonts
          # symbola
          fira
          ibm-plex
          inter
          jost
          public-sans
          julia-mono
          merriweather
          alegreya

          # noto-fonts
          # noto-fonts-cjk
          # noto-fonts-emoji

          # inputs'.ttc-subway-font.packages.ttc-subway
          # inputs'.ttc-subway-font.packages.bloor-yonge-font

          (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })

          font-awesome

          fira-code
          fira-code-symbols
          open-sans
          jetbrains-mono
          siji

          iosevka-bin
        ] ++ (lib.optionals isLinux [
          corefonts
          inconsolata
          liberation_ttf
          dejavu_fonts
          bakoma_ttf
          gentium
          ubuntu_font_family
          terminus_font
        ]) ++ (lib.optionals isMacOS [ ]);
    };
}
