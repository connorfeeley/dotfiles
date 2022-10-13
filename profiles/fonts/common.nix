{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux isMacOS;
in {
  environment.systemPackages = with pkgs; [
    (lib.mkIf isLinux font-manager)
  ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs;
      [
        b612
        barlow
        emacs-all-the-icons-fonts
        symbola
        fira
        ibm-plex
        inter
        jost
        public-sans
        julia-mono
        merriweather
        alegreya

        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji

        ttc-subway

        # FIXME: doesn't exist... yet...
        # (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
        nerdfonts-symbols-only

        font-awesome

        fira-code
        fira-code-symbols
        open-sans
        jetbrains-mono
        siji

        iosevka-bin
      ]
      ++ (lib.optionals isLinux [
        corefonts
        inconsolata
        liberation_ttf
        dejavu_fonts
        bakoma_ttf
        gentium
        ubuntu_font_family
        terminus_font
      ])
      ++ (lib.optionals isMacOS [
        sf-pro
      ]);
  };
}
