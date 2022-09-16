{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    xmonad-contrib = {
      url = "github:xmonad/xmonad-contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, flake-utils, nixpkgs, xmonad, xmonad-contrib }:
  let
    supportedSystems = with flake-utils.lib.system; [
      x86_64-linux
      aarch64-linux
    ];

    overlay = (final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: rec {
          xmobar-config = self.callCabal2nix "xmobar-config"
            ./.
            { };
        });
      });
    });
    overlays = [ overlay xmonad.overlay xmonad-contrib.overlay ];
  in flake-utils.lib.eachSystem supportedSystems (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.xmobar-config p.xmonad-contrib p.xmonad p.xmobar ];
      buildInputs = with pkgs.haskellPackages; [
        cabal-install haskell-language-server hlint ghcid stylish-haskell floskell implicit-hie
      ] ++
      (with pkgs; [
        xorg.libXrandr.dev
      ]);

      withHoogle = true;
    };
    defaultPackage = pkgs.haskellPackages.xmobar-config;
  }) // {
    inherit overlay;
  } ;
}
