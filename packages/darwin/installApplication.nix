{ stdenv
, undmg
, lib
}:
let
  inherit stdenv undmg;
in
{ name, appName, meta, src, propagatedBuildInputs ? [ ] }:
# Install the application from the mounted DMG file.
stdenv.mkDerivation {
  inherit name src propagatedBuildInputs meta;

  nativeBuildInputs = [ undmg ];

  unpackPhase = ''
    set -x
    undmg $src
    pwd
    ls -al
  '';

  installPhase = ''
    mkdir -p $out/Applications
    cp -R "${appName}" $out/Applications
  '';
}
