{ lib, fetchzip, stdenv, unzip, ... }:

stdenv.mkDerivation rec {
  name = "mints";
  version = "110";

  src = fetchzip {
    url = "https://eclecticlightdotcom.files.wordpress.com/2022/12/mints${version}.zip";
    sha256 = "sha256-I4FNe/QpxzPc/OvFzFgjQDWv9HwEWgPIOqfALM/z1rA=";
    stripRoot = false;
  };

  installPhase = ''
    mkdir -p $out/Applications
    cp -r $src/mints${version}/Mints.app $out/Applications
  '';

  meta = {
    description = "A multifunction utility for viewing and exploring MacOS logs.";
    platforms = lib.platforms.darwin;
    homepage = "https://eclecticlight.co/mints-a-multifunction-utility/";
    license = lib.licenses.unfree;
    maintainers = [ lib.maintainers.cfeeley ];
  };
}
