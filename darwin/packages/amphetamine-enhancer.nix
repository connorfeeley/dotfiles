{ lib, fetchurl, installApplication }:
installApplication {
  name = "Amphetamine Enhancer";
  appName = "Amphetamine Enhancer.app";

  src = fetchurl {
    url = "https://github.com/x74353/Amphetamine-Enhancer/raw/master/Releases/Current/Amphetamine%20Enhancer.dmg";
    sha256 = "sha256-qISMBy46rm+J+smftPcbrJy8lrWynMV/fgEjXD45oUw=";
  };

  meta = {
    description = "Add new abilities to the macOS keep-awake utility, Amphetamine.";
    platforms = lib.platforms.darwin;
    homepage = "https://github.com/x74353/Amphetamine-Enhancer";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.cfeeley ];
  };
}
