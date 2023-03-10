{
  description = "Dotfield";

  inputs = {
    ##: --- nixpkgs flavours ----------------------------------------------------------
    nixpkgs.follows = "nixos-unstable";

    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.11";
    # nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
    nixpkgs-darwin.follows = "nixos-unstable";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-21-11.url =
      "github:NixOS/nixpkgs/nixos-21.11"; # Last release w/ sourcetrail

    ##: --- system -------------------------------------------------------------
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    digga = {
      url = "github:divnix/digga";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.darwin.follows = "darwin";
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix.url = "github:montchr/agenix/darwin-support";
    sops-nix = {
      url = "github:pogobanane/sops-nix/feat/home-manager-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # : ~~~ FHS compat ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-autobahn = {
      url = "github:Lassulus/nix-autobahn";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    envfs = {
      url = "github:Mic92/envfs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ##: --- utilities ----------------------------------------------------------
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts = { url = "github:hercules-ci/flake-parts"; };

    nur.url = "github:nix-community/NUR";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nvfetcher.url = "github:berberman/nvfetcher";
    arion = {
      url = "github:hercules-ci/arion";
      inputs.nixpkgs.follows = "nixpkgs";
    }; # FIXME: checks fail on darwin
    nix-serve-ng = {
      url = "github:aristanetworks/nix-serve-ng";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
    };
    nixago = {
      url = "github:nix-community/nixago";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-vscode-server = {
      url = "github:msteen/nixos-vscode-server";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ##: --- sources ------------------------------------------------------------
    mach-nix.url = "github:DavHau/mach-nix/refs/tags/3.5.0";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    deadnix = {
      url = "github:astro/deadnix/refs/tags/v1.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    comma = {
      url = "github:nix-community/comma";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hercules-ci-agent = {
      url = "github:hercules-ci/hercules-ci-agent";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
        nix-darwin.follows = "darwin";
      };
    };
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    ##: --- personal packages --------------------------------------------------
    nurpkgs = {
      url = "github:connorfeeley/nurpkgs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad-config = {
      url = "git+https://git.sr.ht/~cfeeley/xmonad-config";
      inputs.flake-utils.follows = "flake-utils";
    };
    chatgpt-wrapper = {
      url = "git+https://git.sr.ht/~cfeeley/chatgpt-wrapper";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-darwin.follows = "nixpkgs-darwin";
    };
    ttc-subway-font = {
      url = "git+ssh://git@git.sr.ht/~cfeeley/ttc-subway-font";
      inputs.nixpkgs.follows = "nixpkgs";
    }; # Private repo
    nixpkgs-input-leap = {
      url = "sourcehut:~cfeeley/nixpkgs/feat/input-leap";
    };

    ##: --- meta packages ------------------------------------------------------
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin-emacs = { url = "github:c4710n/nix-darwin-emacs"; };
    nixpkgs-overlay-tny = { url = "github:tnytown/nixpkgs-overlay-tny"; inputs.nixpkgs.follows = "nixpkgs"; };
    nix-xilinx = { url = "git+https://git.sr.ht/~cfeeley/nix-xilinx"; };

    ##: --- packages -----------------------------------------------------------
    nickel = {
      url = "github:tweag/nickel";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-nil = { url = "github:oxalica/nil"; };
    nix-init = { url = "github:nix-community/nix-init"; };
    devenv = { url = "github:cachix/devenv/v0.5"; };
    deploy = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-flake = {
      url = "github:antifuchs/deploy-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    prefmanager = {
      url = "github:malob/prefmanager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    tum-dse-config = {
      url = "github:TUM-DSE/doctor-cluster-config";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-unstable.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    neovim-plusultra = { url = "github:jakehamilton/neovim"; };

    ##: --- other --------------------------------------------------------------
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    dwarffs.url = "github:edolstra/dwarffs";
    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };
    firefox-lepton = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };
    modded-minecraft-servers = {
      url = "github:mkaito/nixos-modded-minecraft-servers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-update = {
      url = "github:ryantm/nixpkgs-update";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixos-stable
    , nixpkgs-darwin
    , nixos-unstable
    , nixos-21-11
    , home-manager
    , darwin
    , digga
    , agenix
    , nixos-hardware
    , nix-alien
    , nix-autobahn
    , envfs
    , flake-utils
    , flake-parts
    , nur
    , nixos-generators
    , nvfetcher
    , arion
    , nix-serve-ng
    , nixago
    , nixos-vscode-server
    , mach-nix
    , gitignore
    , nix-colors
    , deadnix
    , comma
    , rnix-lsp
    , nurpkgs
    , xmonad-config
    , chatgpt-wrapper
    , ttc-subway-font
    , nixpkgs-input-leap
    , emacs-overlay
    , darwin-emacs
    , nix-xilinx
    , nickel
    , nix-nil
    , nix-init
    , devenv
    , deploy
    , deploy-flake
    , prefmanager
    , tum-dse-config
    , neovim-plusultra
    , flake-compat
    , dwarffs
    , base16-kitty
    , firefox-lepton
    , modded-minecraft-servers
    , plasma-manager
    , ...
    }@inputs:
    let
      inherit (digga.lib) flattenTree importExportableModules rakeLeaves;
      inherit (flake-utils.lib) eachSystem;
      inherit (flake-utils.lib.system)
        x86_64-linux aarch64-linux

        x86_64-darwin aarch64-darwin;

      supportedSystems = [
        x86_64-linux
        aarch64-linux

        x86_64-darwin
        aarch64-darwin
      ];

      collective = {
        modules = importExportableModules ./modules;
        peers = import ./ops/metadata/peers.nix;
        profiles = rakeLeaves ./profiles;
      };

      # FIXME: split this to shared/nixos/darwin-specific
      overlays = [
        agenix.overlay
        emacs-overlay.overlay
        gitignore.overlay
        nur.overlay
        nvfetcher.overlays.default

        nix-xilinx.overlay

        (final: _prev:
          let packagesFrom = inputAttr: inputAttr.packages.${final.system};
          in
          {
            inherit (packagesFrom self.packages) emacs-plus;
            inherit (packagesFrom inputs.nixpkgs-overlay-tny) emacsMacport;
            inherit (packagesFrom inputs.devenv) devenv;
            inherit (packagesFrom inputs.deploy) deploy-rs;
            inherit (packagesFrom inputs.deploy-flake) deploy-flake;
            inherit (packagesFrom inputs.prefmanager) prefmanager;
            inherit (packagesFrom inputs.nix-nil) nil;
            inherit (packagesFrom inputs.nix-alien) nix-alien;
            inherit (packagesFrom inputs.nix-alien) nix-index-update;
            inherit (packagesFrom inputs.nix-autobahn) nix-autobahn;
            inherit (packagesFrom inputs.nixpkgs-update) nixpkgs-update nixpkgs-update-doc;

            # Personal packages
            inherit (packagesFrom inputs.nurpkgs)
              apple_complete maclaunch toronto-backgrounds;
            inherit (packagesFrom inputs.xmonad-config) xmonad-config;
            inherit (packagesFrom inputs.ttc-subway-font)
              ttc-subway bloor-yonge-font;

            inherit (inputs.nixpkgs-input-leap.legacyPackages.${final.system})
              input-leap;

            nix-init = inputs.nix-init.packages.${final.system}.default;
            emacsGitDarwin =
              inputs.darwin-emacs.packages.${final.system}.default;
            neovim-plusultra =
              inputs.neovim-plusultra.packages.${final.system}.neovim;
          })
        (import ./overlays/tum-dse-config { inherit inputs; })
        (import ./overlays/python { inherit inputs; })
      ];

      commonImports = [
        (digga.lib.importOverlays ./overlays/common)
        (digga.lib.importOverlays ./packages)
      ];
    in
    (digga.lib.mkFlake {
      inherit self inputs supportedSystems;

      channelsConfig = {
        allowUnfree = true;
        allowUnsupportedSystem = false;

        allowBroken = false;

        permittedInsecurePackages = [ "libressl-3.4.3" ];
      };

      ###
      ### Overlay & Channel Configuration
      ###
      channels = {
        nixos-stable = {
          inherit overlays;
          imports = commonImports
          ++ [ (digga.lib.importOverlays ./overlays/stable) ];
        };
        nixpkgs-darwin = {
          imports = commonImports ++ [
            (digga.lib.importOverlays ./overlays/nixpkgs-darwin)
            (digga.lib.importOverlays ./overlays/stable)
          ];
          overlays = overlays ++ [
            (final: _prev: {
              amphetamine-enhancer =
                self.packages.${final.system}.amphetamine-enhancer;
              mints = self.packages.${final.system}.mints;
              hammerspoon = self.packages.${final.system}.hammerspoon;
              native-youtube = self.packages.${final.system}.native-youtube;
              better-display = self.packages.${final.system}.better-display;
            })
          ];
        };
        nixos-unstable = {
          inherit overlays;

          imports = commonImports
          ++ [ (digga.lib.importOverlays ./overlays/nixos-unstable) ];
        };
      };

      sharedOverlays = [
        (_final: prev: {
          __dontExport = true;
          inherit inputs;
          lib = prev.lib.extend (_lfinal: _lprev: { our = self.lib; });
        })
      ];

      ###
      ### Other attributes
      ###
      lib = import ./lib {
        inherit collective;
        lib = digga.lib // nixos-unstable.lib;
      };

      nixos = import ./nixos collective;
      darwin = import ./darwin collective;
      home = import ./home collective;

      devshell = ./shell;

      homeConfigurations = digga.lib.mergeAny
        (digga.lib.mkHomeConfigurations self.darwinConfigurations)
        (digga.lib.mkHomeConfigurations self.nixosConfigurations);

      colmena = import ./colmena.nix { inherit nixpkgs inputs self; };

      deploy.nodes = digga.lib.mkDeployNodes { } {
        workstation = {
          hostname = "workstation";
          sshUser = "cfeeley";
          remoteBuild = true;
          fastConnection = true;
          autoRollback = true;
          magicRollback = true;
          profiles.cfeeley = {
            user = "cfeeley";
            path = deploy.lib.x86_64-linux.activate.nixos
              self.nixosConfigurations.workstation;
          };
        };
        rosy = {
          hostname = "rosy";
          sshUser = "cfeeley";
          remoteBuild = true;
          fastConnection = true;
          autoRollback = true;
          magicRollback = true;
          profiles.system = {
            user = "root";
            path = deploy.lib.aarch64-linux.activate.nixos
              self.nixosConfigurations.rosy;
          };
          profiles.cfeeley = {
            user = "cfeeley";
            path = deploy.lib.aarch64-linux.activate.home-manager
              self.homeConfigurations."cfeeley@rosy";
          };
        };
        h8tsner = {
          hostname = "h8tsner";
          sshUser = "root";
          sshOpts = [ "-p" "26473" ];
          fastConnection = false;
          autoRollback = true;
          magicRollback = true;
          profiles.system = {
            user = "root";
            path = deploy.lib.x86_64-linux.activate.nixos
              self.nixosConfigurations.h8tsner;
          };
          profiles.cfeeley = {
            user = "cfeeley";
            path = deploy.lib.x86_64-linux.activate.home-manager
              self.homeConfigurations."cfeeley@h8tsner";
          };
        };
        # Deploy to 'cfeeley-laptop':
        # - Recommended: deploy .#cfeeley-laptop -- --print-build-logs
        # If there are nix eval errors, then we can tell 'deploy' to skip the flake checks:
        # - Not recommended: deploy --skip-checks .#cfeeley-laptop -- --print-build-logs
        cfeeley-laptop = with (collective.peers.hosts.cfeeley-laptop); {
          hostname = ipv4.address;
          sshUser = "cfeeley";
          fastConnection = true;
          autoRollback = true;
          magicRollback = true;
          profilesOrder = [ "cfeeley" ];
          profiles.cfeeley = {
            user = "cfeeley";
            path = deploy.lib.x86_64-linux.activate.home-manager
              self.homeConfigurationsPortable.x86_64-linux."cfeeley@cfeeley-laptop";
          };
        };
      };

      overlays = rec {
        # Helper function to install DMGs
        installApplication = self.overlays."nixpkgs-darwin/installApplication";
        darwin-packages = nixpkgs.lib.composeManyExtensions [
          installApplication

          self.overlays."nixpkgs-darwin/macports"
          self.overlays."nixpkgs-darwin/emacs-plus"
        ];
        linux-packages = nixpkgs.lib.composeManyExtensions [
          # FIXME(darwin): causes 'nix flake show' to error
          # self.overlays."nixos-stable/xmonad-config"

          self.overlays."nixos-stable/xsct"
          self.overlays."nixos-stable/mdio-tools"
          self.overlays."nixos-stable/aranet4"
          # self.overlays."nixos-stable/fildem-global-menu"
        ];
      };
    }) //
    # Generate attrs for each system: (formatter.<system>)
    (eachSystem supportedSystems
      (system: { formatter = nixpkgs.legacyPackages.${system}.nixpkgs-fmt; }))
    //
    # Generate attrs for each system: (formatter.<system>)
    {
      packages =
        let
          mkLinuxPackages = system:
            let
              pkgs = import nixpkgs {
                inherit system;
                overlays = [ self.overlays.linux-packages ];
                config.allowUnfree = true;
              };
            in
            {
              # FIXME(darwin): causes 'nix flake show' to error
              inherit (pkgs) xsct mdio-tools aranet4;
            };

          mkDarwinPackages = system:
            let
              pkgs = import nixpkgs {
                inherit system;
                overlays = [ self.overlays.darwin-packages ];
                config.allowUnfree = true;
              };
            in
            {
              inherit (pkgs)
                macports amphetamine-enhancer mints hammerspoon native-youtube
                better-display;
            } // (builtins.mapAttrs
              (_n: v: pkgs.callPackage v { inherit (pkgs) installApplication; })
              (flattenTree (rakeLeaves ./darwin/packages)));
        in
        {
          x86_64-linux = mkLinuxPackages "x86_64-linux";
          aarch64-linux = mkLinuxPackages "aarch64-linux";
          x86_64-darwin = mkDarwinPackages "x86_64-darwin";
          aarch64-darwin = mkDarwinPackages "aarch64-darwin";
        };

      templates =
        let
          poetry2nix = {
            path = ./templates/poetry2nix;
            description = "poetry2nix template";
            welcomeText =
              "Set project name: sed --in-place 's/poetry2nixTemplate/<project-name>/g' flake.nix";
          };
        in
        {
          inherit poetry2nix;
          default = poetry2nix;
        };
    };

  # Automatic nix.conf settings (accepted automatically when 'accept-flake-config = true')
  nixConfig.extra-experimental-features = "nix-command flakes";
  nixConfig.extra-substituters = [
    "https://cache.nixos.org/"
    "https://nix-community.cachix.org"
    "https://cfeeley.cachix.org"
  ];
  nixConfig.extra-trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "cfeeley.cachix.org-1:b+RrHsy/4WWys2o6T4YyF66OhdiZUF/R/N46JcS0HJU="
  ];
}
