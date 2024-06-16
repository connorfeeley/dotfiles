{ lib, pkgs, ... }:
let inherit (pkgs.stdenv.hostPlatform) isLinux;
in {
  environment.systemPackages = lib.optionals isLinux [ pkgs.font-manager ];

  fonts = {
    fontDir = { enable = lib.mkIf isLinux true; };
    packages = (with pkgs; [
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
    ]) ++ (lib.optionals isLinux (with pkgs; [
      corefonts
      inconsolata
      liberation_ttf
      dejavu_fonts
      bakoma_ttf
      gentium
      ubuntu_font_family
      terminus_font
    ]));
  };
}
