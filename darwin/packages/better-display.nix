{ lib, fetchurl, installApplication }:
installApplication {
  name = "BetterDisplay";
  appName = "BetterDisplay.app";

  src = fetchurl {
    url =
      "https://github.com/waydabber/BetterDisplay/releases/download/v1.4.3/BetterDisplay-v1.4.3.dmg";
    sha256 = "sha256-bio7I6HXyEoXiL/7UaxnyNVm93e/TO5WJEtv++3TUOo=";
  };

  meta = {
    description = "Unlock your displays on your Mac";
    platforms = lib.platforms.darwin;
    homepage = "https://github.com/waydabber/BetterDisplay";
    license = lib.licenses.unfree;
    maintainers = [ lib.maintainers.cfeeley ];
  };
}
