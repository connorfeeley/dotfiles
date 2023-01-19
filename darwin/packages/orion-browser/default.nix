{ lib, fetchurl, installApplication }:
installApplication {
  name = "Orion";
  appName = "Orion.app";

  src = fetchurl {
    url = "https://cdn.kagi.com/downloads/OrionInstaller.dmg";
    sha256 = "0hgwfv93whrmsxqcm4yn8chd0v1vili0yxf2w2qwg877lk73k8xl";
  };

  meta = {
    description = "Native lightweight WebKit browser for MacOS.";
    platforms = lib.platforms.darwin;
    homepage = "https://browser.kagi.com";
    license = lib.licenses.unfree;
    maintainers = [ lib.maintainers.cfeeley ];
  };
}
