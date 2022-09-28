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
    fontconfig = {
      localConf = ''
    <!-- Artificial oblique for fonts without an italic or oblique version -->
    <match target="font" >
        <!-- check to see if the font is roman -->
        <test name="slant" >
            <const>roman</const>
        </test>
        <!-- check to see if the pattern requested non-roman -->
        <test target="pattern" compare="not_eq" name="slant" >
            <const>roman</const>
        </test>
        <!-- multiply the matrix to slant the font -->
        <edit mode="assign" name="matrix" >
            <times>
                <name>matrix</name>
                <matrix>
                    <double>1</double>
                    <double>0.2</double>
                    <double>0</double>
                    <double>1</double>
                </matrix>
            </times>
        </edit>
        <!-- pretend the font is oblique now -->
        <edit mode="assign" name="slant" >
            <const>oblique</const>
        </edit>
    </match>
      '';
    };
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
  };
}
