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
  outputs =
    { self, flake-utils, nixpkgs, xmonad-config, xmonad, xmonad-contrib }:
    let
      supportedSystems = with flake-utils.lib.system; [
        x86_64-linux
        aarch64-linux
      ];

      overlay = (final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
            (self: super: rec {
              xmonad-config = self.callCabal2nix "xmonad-config" ./. { };
            });
        });

        # xmonad-config = final.haskellPackages.callCabal2nix "xmonad-config" ./. { };
      });
      overlays = [ xmonad.overlay xmonad-contrib.overlay overlay ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          extraOutputsToInstall = [ "dev" "doc" "src" ];
          packages = p: [ p.xmonad-config ];

          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            ghcid
            fourmolu
            implicit-hie
          ];
          separateDebugOutput = true;
          withHoogle = true;
        };
        defaultPackage = pkgs.haskellPackages.xmonad-config;
      }) // {
        inherit overlay;
      };
}
