{ lib, stdenv, fetchzip, fetchFromGitHub, ... }:
let
  spoonSources = fetchFromGitHub {
    owner = "Hammerspoon";
    repo = "Spoons";
    rev = "b36b6f6c654b787be11b6acde1f9ec4fcd217669";
    sha256 = "sha256-ShfiwQluh89aFmqSKL4Q5X9BECm3qP6jreYGIV0Qb/g=";
  };
in
stdenv.mkDerivation rec {
  pname = "hammerspoon";
  version = "0.9.96";

  src = fetchzip {
    url = "https://github.com/Hammerspoon/hammerspoon/releases/download/${version}/Hammerspoon-${version}.zip";
    sha256 = "sha256-3euDPI0qJD3Nj7mUAUKDab/ZFQTcRVU68oNmQe+DxKQ=";
  };

  installPhase = ''
    mkdir -p $out/Applications/Hammerspoon.app
    mv ./* $out/Applications/Hammerspoon.app
    chmod +x "$out/Applications/Hammerspoon.app/Contents/MacOS/Hammerspoon"
  '';

  passthru.spoons = spoonSources;

  meta = {
    description = "Staggeringly powerful macOS desktop automation with Lua";
    license = lib.licenses.mit;
    homepage = "https://www.hammerspoon.org";
    platforms = lib.platforms.darwin;
  };
}
