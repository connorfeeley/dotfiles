{ lib, stdenv, fetchzip, ... }:

stdenv.mkDerivation rec {
  pname = "native-youtube";
  version = "3.1";

  src = fetchzip {
    url = "https://github.com/Aayush9029/NativeYoutube/releases/download/v${version}/NativeYoutube.app.zip";
    sha256 = "sha256-3QLVEMAAKJNmyicld/vjnJQKYF5LGdRJQrovZ2Yrc38=";
  };

  installPhase = ''
    mkdir -p "$out/Applications/Native YouTube.app"
    mv * "$out/Applications/Native YouTube.app"
  '';

  meta = {
    description = "Native YouTube app for macOS";
    license = lib.licenses.mit;
    homepage = "https://github.com/Aayush9029/NativeYoutube";
    platforms = lib.platforms.darwin;
  };
}
