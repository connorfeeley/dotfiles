{ fetchurl, lib, stdenvNoCC, undmg, unzip }:
let
  # TODO: break this out into a common function
  # Source: https://discourse.nixos.org/t/feedback-darwin-installapplication/11324
  installApplication =
    { app ? "./", description, homepage, license, maintainers, name, sha256, url, version, sourceRoot ? null, postInstall ? "", ... }:
    stdenvNoCC.mkDerivation {
      inherit name version sourceRoot;

      nativeBuildInputs = [ undmg unzip ];

      src = fetchurl {
        inherit url sha256;
      };

      phases = [ "unpackPhase" "installPhase" ];

      installPhase = ''
        set -x
        mkdir -p "$out/Applications/${app}"
        mv * "$out/Applications/${app}"
      '' + postInstall;

      meta = with lib; {
        inherit description;
        inherit homepage;
        license = licenses."${license}";
        maintainers = forEach maintainers (_x: maintainers."${maintainer}");
        platforms = platforms.darwin;
      };
    };
in
installApplication {
  name = "Amphetamine Enhancer";
  app = "./Amphetamine Enhancer.app";
  sourceRoot = "Amphetamine Enhancer.app";
  version = "1.0";

  url = "https://github.com/x74353/Amphetamine-Enhancer/raw/master/Releases/Current/Amphetamine%20Enhancer.dmg";
  sha256 = "sha256-qISMBy46rm+J+smftPcbrJy8lrWynMV/fgEjXD45oUw=";

  description = "Add new abilities to the macOS keep-awake utility, Amphetamine.";
  homepage = "https://github.com/x74353/Amphetamine-Enhancer";
  license = "mit";
  maintainers = [ lib.maintainers.cfeeley ];
  broken = true;
}
