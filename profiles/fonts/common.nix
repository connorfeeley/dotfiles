{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux isMacOS;
in
{
  environment.systemPackages = with pkgs; [
    (lib.mkIf isLinux font-manager)
  ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs;
      [
        b612
        barlow
        emacs-all-the-icons-fonts
        fira
        ibm-plex
        inter
        jost
        public-sans

        ttc-subway

        # FIXME: doesn't exist... yet...
        # (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
        nerdfonts-symbols-only

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
    fontconfig = {
      enable = true;
      localConf = ''
        <!-- use a less horrible font substition for pdfs such as https://www.bkent.net/Doc/mdarchiv.pdf -->
        <match target="font">
          <test qual="any" name="family"><string>Toronto Subway</string></test>
          <test name="slant" compare="more_eq"><int>100</int></test>
        </match>
      '';
    };
  };
}
