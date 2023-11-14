{ lib, stdenv, makeWrapper, zsh, python3Packages, ... }:

stdenv.mkDerivation {
  name = "pwrbar";

  src = ./.;

  buildInputs = [ makeWrapper ];

  propagatedBuildInputs = [ zsh python3Packages.python-kasa ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src/pwrbar $out/bin/pwrbar
  '';

  preFixup = ''
    wrapProgram $out/bin/pwrbar \
      --prefix PATH : ${python3Packages.python-kasa}/bin
  '';

  meta = {
    description = "Helper script for Kasa WiFi power bar";
    license = lib.licenses.mit;
    platforms =
      [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
    maintainers = [ ];
  };
}
