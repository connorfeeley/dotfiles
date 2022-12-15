{ lib, requireFile, installApplication }:
installApplication {
  name = "VMware Fusion";
  appName = "VMware Fusion.app";

  src = requireFile {
    name = "VMware-Fusion-13.0.0-20802013_universal.dmg";
    sha256 = "40bb9fbd4b2a18b48138a7fb3285d89187d50caab10506cff81b367b6edc858d";
  };

  meta = {
    description = "VMware Fusion";
    platforms = lib.platforms.darwin;
    maintainers = [ lib.maintainers.cfeeley ];
  };
}
