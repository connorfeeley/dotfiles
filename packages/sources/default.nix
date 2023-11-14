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

  git-submodule-rewrite = stdenv.mkDerivation {
    name = "git-submodule-rewrite";
    src = ./git-submodule-rewrite;
    installPhase = ''
      mkdir -p $out/bin
      cp bin/* $out/bin/
    '';
  };
}
