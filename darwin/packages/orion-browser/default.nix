{ lib, fetchurl, fetchzip, stdenv, installApplication, unzip, nix-info }:
let
  fetchOrion = { macosRelease, version, sha256 }: fetchurl {
    url = "https://browser.kagi.com/updates/${macosRelease}/${toString version}.zip";
    inherit sha256;
    name = "orion-${macosRelease}-${toString version}.zip";
  };

  distVersions = {
    "12_0" = {
      version = 122;
      sha256 = "sha256-LcFOJou7ZODI4MVtVbTbSlB+r/PbifFbiLcYXlVhQxc=";
    };
    "13" = {
      version = 122;
      sha256 = "sha256-gz69baE0jVtHrEaGVNnV4SdQDlXhq8rm/S70pazcFN0=";
    };
  };

  # majorVersion = builtins.head (builtins.splitVersion orionVersion);
in
stdenv.mkDerivation rec {
  name = "orion";
  version = 122;
  # Documentation: https://help.kagi.com/orion/welcome.html
  srcs = [
    (fetchOrion {
      inherit version;
      macosRelease = "13_0";
      sha256 = "sha256-Y1sy9P3VqLttojju1olbVxYzuWXR04H9WwMAMXi1udY=";
    })
    (fetchOrion {
      inherit version;
      macosRelease = "12_0";
      sha256 = "sha256-eZyzYsqtMCC+Seg2ln7r+DWlPOFnCXhT0qN648s9vNo=";
    })
  ];

  nativeBuildInputs = [ unzip ];

  unpackPhase = ''
    # mkdir -p $out
    # for src in $srcs; do
    #   local extractedDir=$(basename $src .zip)
    #   unzip $src -d $extractedDir
    # done
  '';

  installPhase = ''
    macosVersion=$(sw_vers -productVersion)
    case "$macosVersion" in
      12.*)
        macosRelease="12_0"
        ;;
      13.*)
        macosRelease="13_0"
        ;;
      *)
        echo "Unsupported macOS version: $macosVersion"
        exit 1
        ;;
    esac

    mkdir -p $out/Applications

    for src in $srcs; do
      case $src in
        *$macosVersion.zip)
          unzip $src -d $out/Applications
          ;;
        *)
          echo "Couldn't match src"
          exit 1
          ;;
      esac
    done
  '';

  meta = {
    description = "Native lightweight WebKit browser for MacOS.";
    platforms = lib.platforms.darwin;
    homepage = "https://browser.kagi.com";
    license = lib.licenses.unfree;
    maintainers = [ lib.maintainers.cfeeley ];
  };
}
