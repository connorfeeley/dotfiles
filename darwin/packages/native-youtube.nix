{ lib, stdenv, fetchzip, ... }:

stdenv.mkDerivation rec {
  # NOTE: 3.1 requires MacOS Ventura
  version = "2.2.3";
  pname = "native-youtube";

  src = fetchzip {
    url = "https://github.com/Aayush9029/NativeYoutube/releases/download/v${version}/NativeYoutube.app.zip";
    sha256 = "sha256-kzdM4Q3pKEcdPNRTIo+cC9Nd6m0um9+wd+hVp0kYpic=";
  };

  installPhase = ''
    mkdir -p "$out/Applications/NativeYoutube.app"
    cp -r * "$out/Applications/NativeYoutube.app"
  '';

  meta = {
    description = "Native YouTube app for macOS";
    license = lib.licenses.mit;
    homepage = "https://github.com/Aayush9029/NativeYoutube";
    platforms = lib.platforms.darwin;
  };
}
