{
  description = "Dotfield";

  inputs = {
    stable.url = "github:nixos/nixpkgs/release-21.05";
    latest.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    digga.url = "github:divnix/digga";
    digga.inputs.nixpkgs.follows = "latest";
    digga.inputs.nixlib.follows = "latest";
    digga.inputs.home-manager.follows = "home-manager";

    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    nur.url = "github:nix-community/NUR";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "latest";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "latest";

    emacs.url = "github:montchr/emacs/develop";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    rnix-lsp.url = "github:nix-community/rnix-lsp";
    rnix-lsp.inputs.nixpkgs.follows = "nixpkgs";

    nvfetcher.url = "github:berberman/nvfetcher";
    nvfetcher.inputs.nixpkgs.follows = "latest";
    nvfetcher.inputs.flake-utils.follows = "digga/flake-utils-plus/flake-utils";

    firefox-lepton = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };

    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };

    nixpkgs.follows = "latest";
    nixlib.follows = "digga/nixlib";
    blank.follows = "digga/blank";
    utils.follows = "digga/flake-utils-plus";
  };

  outputs =
    { self
    , darwin
    , digga
    , emacs
    , emacs-overlay
    , home-manager
    , utils
    , stable
    , latest
    , nixlib
    , nur
    , nvfetcher
    , ...
    } @ inputs:
    let
      hostConfigs = digga.lib.rakeLeaves ./hosts;
      systemProfiles = digga.lib.rakeLeaves ./profiles;
      userProfiles = digga.lib.rakeLeaves ./users/profiles;
      suites = rec {
        base = [
          systemProfiles.core
          systemProfiles.networking.common
          userProfiles.zsh
        ];
        developer = suites.base ++ [
          userProfiles.emacs
        ];
        darwin-minimal = suites.base ++ [
          systemProfiles.darwin.common
        ];
        darwin = suites.darwin-minimal ++ [
          systemProfiles.darwin.system-defaults
          userProfiles.darwin.gui
          userProfiles.darwin.keyboard
        ];
        personal = [
          userProfiles.gnupg
          userProfiles.mail
          userProfiles.pass
        ];
        vagrant = [
          systemProfiles.langs.ruby
        ];
      };
    in
    utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig = {
        allowUnfree = true;
      };

      channels = {
        stable = { };
        latest = { };
      };

      lib = import ./lib { lib = digga.lib // latest.lib; };

      sharedOverlays = [
        (import ./overlays/yabai.nix)
        emacs.overlay
        nur.overlay
        nvfetcher.overlay
        (import ./pkgs/default.nix)
        (final: prev: {
          nix-direnv = (prev.nix-direnv.override { enableFlakes = true; });
          pragmatapro = (prev.callPackage ./pkgs/pragmatapro.nix { });
        })
        (final: prev: {
          __dontExport = true;
          lib = prev.lib.extend (lfinal: lprev: {
            our = self.lib;
          });
        })
      ];

      hostDefaults = {
        channelName = "latest";
        extraArgs = { inherit utils inputs; };
        specialArgs = { inherit suites systemProfiles userProfiles; };
        system = "x86_64-darwin";
        output = "darwinConfigurations";
        builder = darwin.lib.darwinSystem;
        modules = [
          ./modules
          ./modules/dotfield.nix
          ./users/primary-user
        ] ++ (builtins.attrValues (digga.lib.flattenTree
          (digga.lib.rakeLeaves ./users/modules)));
      };

      hosts =
        let
          mkDarwinHost = name:
            { minimal ? false
            , extraSuites ? [ ]
            }: {
              modules = (if minimal then suites.darwin-minimal else suites.darwin)
                ++ extraSuites
                ++ [
                hostConfigs.${name}
                home-manager.darwinModules.home-manager
              ];
            };
        in
        {
          HodgePodge = (mkDarwinHost "HodgePodge" {
            extraSuites = suites.personal ++ suites.developer;
          });
          alleymon = (mkDarwinHost "alleymon" {
            extraSuites = suites.personal ++ suites.developer ++ suites.vagrant;
          });
          ghaDarwin = (mkDarwinHost "ghaDarwin" { minimal = true; });
        };

      # Shortcuts
      HodgePodge = self.darwinConfigurations.HodgePodge.system;
      alleymon = self.darwinConfigurations.alleymon.system;
      ghaDarwin = self.darwinConfigurations.ghaDarwin.system;

    };
}
