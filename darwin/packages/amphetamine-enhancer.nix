{ fetchurl, lib, stdenvNoCC, undmg, unzip }:
let
  # TODO: break this out into a common function
  # Source: https://discourse.nixos.org/t/feedback-darwin-installapplication/11324
  installApplication =
    { app ? name, description, homepage, license, maintainers, name, sha256, url, version, postInstall ? "", ... }:
    let
      appEscaped = lib.escapeShellArg app;
    in
    stdenvNoCC.mkDerivation {
      inherit name version;

      nativeBuildInputs = [ undmg unzip ];

      sourceRoot = appEscaped;
      src = fetchurl {
        inherit url sha256;
      };

      phases = [ "unpackPhase" "installPhase" ];
      preUnpack = "set -x; ls -al";

      installPhase = ''
        set -x
        mkdir -p "$out/Applications/${appEscaped}"
        mv * "$out/Applications/${appEscaped}"
      '' + postInstall;

      meta = with lib; {
        description = description;
        homepage = homepage;
        license = licenses."${license}";
        maintainers = forEach maintainers (x: maintainers."${maintainer}");
        platforms = platforms.darwin;
      };
    };
in
installApplication {
  name = "Amphetamine Enhancer";
  app = "Amphetamine\\\\ Enhancer.dmg";
  version = "1.0";

  url = "https://github.com/x74353/Amphetamine-Enhancer/raw/master/Releases/Current/Amphetamine%20Enhancer.dmg";
  sha256 = "sha256-qISMBy46rm+J+smftPcbrJy8lrWynMV/fgEjXD45oUw=";

  description = " Add new abilities to the macOS keep-awake utility, Amphetamine.";
  homepage = "https://github.com/x74353/Amphetamine-Enhancer";
  license = "mit";
  maintainers = [ lib.maintainers.cfeeley ];
}
