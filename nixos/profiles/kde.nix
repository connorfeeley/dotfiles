{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv) isLinux;
  myTheme = with pkgs;
    stdenvNoCC.mkDerivation {
      pname = "artim-dark";
      version = "unstable-2021-12-29";

      src = fetchFromGitHub {
        owner = "Mrcuve0";
        repo = "Aritim-Dark";
        rev = "99cd330a1ab4814260e28f15431e3338a1103668";
        hash = "sha256-xGnw5KpXbVyDdTuAkav1Hec6bitpZdPzZk0xv7WHTdY=";
      };

      dontBuild = true;
      installPhase = ''
        mkdir -p $out/share/plasma/desktoptheme
        cp -R KDE/plasmaTheme/Aritim-Dark* $out/share/plasma/desktoptheme
        mkdir -p $out/share/aurorae/themes
        cp -R KDE/auroraeTheme $out/share/aurorae/themes/Aritim-Dark
        mkdir -p $out/share/color-schemes
        cp -R KDE/colorScheme/*.colors $out/share/color-schemes
        mkdir -p $out/share/plasma/look-and-feel
        cp -R KDE/globalTheme $out/share/plasma/look-and-feel/Aritim-Dark
        mkdir -p $out/share/themes
        cp -R GTK $out/share/themes/Aritim-Dark
      '';

      meta = {
        description =
          "Dark theme deeply inspired by the Ayu Dark color palette";
        homepage = "https://github.com/Mrcuve0/Aritim-Dark";
        license = with lib.licenses; [ gpl3Only ];
        platforms = lib.platforms.unix;
        maintainers = [ lib.maintainers.pasqui23 ];
      };
    };
in
{
  ###
  ### KDE
  ###
  environment.systemPackages = with pkgs; [ myTheme variety ];
  services.xserver.desktopManager.plasma5 = {
    enable = true;
    useQtScaling = true;
    runUsingSystemd = false;
  };

  services.xserver.displayManager = {
    # defaultSession = "plasma+xmonad";
    session =
      let
        localPath =
          "/home/cfeeley/source/xmonad-config/dist-newstyle/build/x86_64-linux/ghc-9.0.2/xmonad-config-0.1/x/xmonad/build/xmonad/xmonad";
        defaultPath = "${pkgs.xmonad-config}/bin/xmonad";
      in
      [{
        manage = "desktop";
        name = "plasma+xmonad";
        start = ''
          export KDEWM=${localPath}
          exec env KDEWM=${localPath} ${pkgs.plasma-workspace}/bin/startplasma-x11
        '';
      }];
  };

  qt.platformTheme = "gnome";
}
