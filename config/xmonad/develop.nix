pkgs: devInputs: devInputs // {
  nativeBuildInputs = with pkgs.haskellPackages;
    [ cabal-install hlint ghcid ormolu implicit-hie haskell-language-server ];
}
