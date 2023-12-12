{callPackage, stdenv, ...}: {
  # nvfetcher sources
  sources = callPackage ./_sources/generated.nix { };

  ediff-tool = stdenv.mkDerivation {
    name = "ediff-tool";
    src = ./ediff-tool;
    installPhase = ''
      mkdir -p $out/bin
      cp bin/* $out/bin/
    '';
  };
}
