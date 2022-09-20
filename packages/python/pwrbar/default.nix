{ config
, lib
, stdenv
, makeWrapper
, zsh
, python-kasa
, ...
}:

stdenv.mkDerivation {
  name = "pwrbar";

  src = ./.;

  buildInputs = [
    makeWrapper
  ];

  propagatedBuildInputs = [
    zsh
    python-kasa
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src/pwrbar $out/bin/pwrbar
  '';

  preFixup = ''
    wrapProgram $out/bin/pwrbar \
      --prefix PATH : ${python-kasa}/bin
  '';

  meta = {
    description = "Helper script for Kasa WiFi power bar";
    license = lib.licenses.mit;
    platforms = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
    maintainers = [ ];
  };
}
