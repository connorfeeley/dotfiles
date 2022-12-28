{
  description = "Dotfield";

  inputs = {
    ##: --- nixpkgs flavours ----------------------------------------------------------
    nixpkgs.follows = "nixos-stable";

    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-21-11.url = "github:NixOS/nixpkgs/nixos-21.11"; # Last release w/ sourcetrail

    ##: --- system -------------------------------------------------------------
    home-manager = { url = "github:nix-community/home-manager/release-22.11"; inputs.nixpkgs.follows = "nixpkgs"; };
    darwin = { url = "github:LnL7/nix-darwin"; inputs.nixpkgs.follows = "nixpkgs"; };
    digga = {
      url = "github:divnix/digga/home-manager-22.11"; # FIXME: eventually track main
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.darwin.follows = "darwin";
    };
    agenix.url = "github:montchr/agenix/darwin-support";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # : ~~~ FHS compat ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    nix-alien = { url = "github:thiagokokada/nix-alien"; inputs.nixpkgs.follows = "nixpkgs"; };
    nix-autobahn = { url = "github:Lassulus/nix-autobahn"; inputs.nixpkgs.follows = "nixpkgs"; };
    envfs = { url = "github:Mic92/envfs"; inputs.nixpkgs.follows = "nixpkgs"; };

    ##: --- utilities ----------------------------------------------------------
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts = { url = "github:hercules-ci/flake-parts"; };

    nur.url = "github:nix-community/NUR";
    nixos-generators = { url = "github:nix-community/nixos-generators"; inputs.nixpkgs.follows = "nixpkgs"; };
    nvfetcher.url = "github:berberman/nvfetcher";
    # arion = { url = "github:hercules-ci/arion"; };
    nix-serve-ng = { url = "github:aristanetworks/nix-serve-ng"; inputs.nixpkgs.follows = "nixpkgs"; inputs.utils.follows = "flake-utils"; };
    nixago = { url = "github:nix-community/nixago"; inputs.nixpkgs.follows = "nixpkgs"; };
    nixos-vscode-server = { url = "github:msteen/nixos-vscode-server"; inputs.nixpkgs.follows = "nixpkgs"; };

    ##: --- sources ------------------------------------------------------------
    mach-nix.url = "github:DavHau/mach-nix/refs/tags/3.5.0";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    deadnix = { url = "github:astro/deadnix/refs/tags/v1.0.0"; inputs.nixpkgs.follows = "nixpkgs"; };
    comma = { url = "github:nix-community/comma"; inputs.nixpkgs.follows = "nixpkgs"; };
    nix-index-database = { url = "github:Mic92/nix-index-database"; inputs.nixpkgs.follows = "nixpkgs"; };
    rnix-lsp = { url = "github:nix-community/rnix-lsp"; inputs.nixpkgs.follows = "nixpkgs"; };
    hercules-ci-agent = { url = "github:hercules-ci/hercules-ci-agent"; inputs = { flake-parts.follows = "flake-parts"; nix-darwin.follows = "darwin"; }; };

    ##: --- personal packages --------------------------------------------------
    xmonad-config = { url = "git+https://git.sr.ht/~cfeeley/xmonad-config"; inputs.flake-utils.follows = "flake-utils"; };
    chatgpt-wrapper = { url = "git+https://git.sr.ht/~cfeeley/chatgpt-wrapper"; inputs.flake-utils.follows = "flake-utils"; inputs.nixpkgs.follows = "nixpkgs"; inputs.nixpkgs-darwin.follows = "nixpkgs-darwin"; };
    ttc-subway-font = { url = "git+ssh://git@git.sr.ht/~cfeeley/ttc-subway-font"; inputs.nixpkgs.follows = "nixpkgs"; }; # Private repo

    ##: --- meta packages ------------------------------------------------------
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; inputs.nixpkgs.follows = "nixpkgs"; };
    nix-xilinx = { url = "gitlab:doronbehar/nix-xilinx"; };

    ##: --- packages -----------------------------------------------------------
    nickel = { url = "github:tweag/nickel"; inputs.nixpkgs.follows = "nixpkgs"; };
    nix-nil = { url = "github:oxalica/nil"; inputs.nixpkgs.follows = "nixpkgs"; };
    devenv = { url = "github:cachix/devenv/v0.5"; };
    deploy = { url = "github:serokell/deploy-rs"; inputs.nixpkgs.follows = "nixpkgs"; };
    deploy-flake = { url = "github:antifuchs/deploy-flake"; inputs.nixpkgs.follows = "nixpkgs"; };
    nixvim = { url = "github:pta2002/nixvim"; inputs.nixpkgs.follows = "nixpkgs"; };
    prefmanager.url = "github:malob/prefmanager";
    tum-dse-config = { url = "github:TUM-DSE/doctor-cluster-config"; inputs.nixpkgs.follows = "nixpkgs"; inputs.nixpkgs-unstable.follows = "nixpkgs"; inputs.flake-parts.follows = "flake-parts"; };

    ##: --- other --------------------------------------------------------------
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    dwarffs.url = "github:edolstra/dwarffs";
    base16-kitty = { url = "github:kdrag0n/base16-kitty"; flake = false; };
    firefox-lepton = { url = "github:black7375/Firefox-UI-Fix"; flake = false; };
    modded-minecraft-servers = { url = "github:mkaito/nixos-modded-minecraft-servers"; inputs.nixpkgs.follows = "nixpkgs"; };
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
    # , arion
    , nix-serve-ng
    , nixago
    , nixos-vscode-server
    , mach-nix
    , gitignore
    , nix-colors
    , deadnix
    , comma
    , rnix-lsp
    , xmonad-config
    , chatgpt-wrapper
    , ttc-subway-font
    , emacs-overlay
    , nix-xilinx
    , nickel
    , nix-nil
    , devenv
    , deploy
    , deploy-flake
    , nixvim
    , prefmanager
    , tum-dse-config
    , flake-compat
    , dwarffs
    , base16-kitty
    , firefox-lepton
    , modded-minecraft-servers
    , ...
    } @ inputs:
    let
      inherit (digga.lib)
        flattenTree
        importExportableModules
        rakeLeaves
        ;
      inherit (flake-utils.lib)
        eachSystem
        ;
      inherit (flake-utils.lib.system)
        x86_64-linux
        aarch64-linux

        x86_64-darwin
        aarch64-darwin
        ;

      supportedSystems = [
        x86_64-linux
        aarch64-linux

        x86_64-darwin

        # FIXME: Something in this flake's chain of dependencies triggers a build
        # failure when `aarch64-darwin` is added to `supportedSystems`,
        # specifically due to `pyopenssl`. Many python packages will not build on
        # this system due to the broken `pyopenssl` dependency.
        #
        # As of 2022-08-20, it appears most of these issues have been fixed, but
        # some packages have still caused errors:
        #
        # - promnesia
        # - yubikey-manager and/or yubikey-personalization
        #
        # [Updated: 2022-08-20]
        # https://github.com/NixOS/nixpkgs/issues/175875
        # https://github.com/pyca/pyopenssl/issues/873
        aarch64-darwin
      ];

      darwinSystems = [ x86_64-darwin aarch64-darwin ];

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
        nvfetcher.overlay
        nix-nil.overlays.default

        xmonad-config.overlays.default

        ttc-subway-font.overlay

        nix-xilinx.overlay

        (final: _prev:
          let
            packagesFrom = inputAttr: inputAttr.packages.${final.system};
          in
          {
            inherit (packagesFrom self.packages) emacs-plus;
            inherit (packagesFrom inputs.devenv) devenv;
            inherit (packagesFrom inputs.deploy) deploy-rs;
            inherit (packagesFrom inputs.deploy-flake) deploy-flake;
            inherit (packagesFrom inputs.prefmanager) prefmanager;
            inherit (packagesFrom inputs.xmonad-config) xmonad-config;
            inherit (packagesFrom inputs.xmonad-config) xmobar-config;
            inherit (packagesFrom inputs.nix-alien) nix-alien;
            inherit (packagesFrom inputs.nix-alien) nix-index-update;
            inherit (packagesFrom inputs.nix-autobahn) nix-autobahn;
          }
        )
        (import ./overlays/tum-dse-config { inherit inputs; })
        (import ./overlays/python { inherit inputs; })
      ];

      commonImports = [
        (digga.lib.importOverlays ./overlays/common)
        (digga.lib.importOverlays ./packages)
      ];
    in
    (digga.lib.mkFlake {
      inherit
        self
        inputs
        supportedSystems
        ;

      channelsConfig = {
        allowUnfree = true;
        allowUnsupportedSystem = false;

        allowBroken = false;

        permittedInsecurePackages = [
          # nixpkgs: pkgs/development/tools/poetry2nix/poetry2nix/mk-poetry-dep.nix
          "python2.7-pyjwt-1.7.1"
          "python3.10-poetry-1.2.2"
          "python3.10-certifi-2022.9.24"
        ];
      };

      ###
      ### Overlay & Channel Configuration
      ###
      channels = {
        nixos-stable = {
          inherit overlays;
          imports = commonImports ++
          [
            (digga.lib.importOverlays ./overlays/stable)
          ];
        };
        nixpkgs-darwin = {
          imports = commonImports ++
          [
            (digga.lib.importOverlays ./overlays/nixpkgs-darwin)
            (digga.lib.importOverlays ./overlays/stable)
          ];
          overlays =
            overlays ++ [
              (final: _prev: {
                amphetamine-enhancer = self.packages.${final.system}.amphetamine-enhancer;
              })
            ];
        };
        nixos-unstable = {
          inherit overlays;

          imports = commonImports ++
          [
            (digga.lib.importOverlays ./overlays/nixos-unstable)
          ];
        };
      };

      sharedOverlays = [
        (_final: prev: {
          __dontExport = true;
          inherit inputs;
          lib = prev.lib.extend (_lfinal: _lprev: {
            our = self.lib;
          });
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
        (digga.lib.mkHomeConfigurations self.nixosConfigurations)
      ;

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
        # rosy = {
        #   hostname = "rosy";
        #   sshUser = "cfeeley";
        #   remoteBuild = true;
        #   fastConnection = true;
        #   autoRollback = true;
        #   magicRollback = true;
        #   profiles.system = {
        #     user = "root";
        #     path = deploy.lib.aarch64-linux.activate.nixos
        #       self.nixosConfigurations.rosy;
        #   };
        #   profiles.cfeeley = {
        #     user = "cfeeley";
        #     path = deploy.lib.aarch64-linux.activate.home-manager
        #       self.homeConfigurations."cfeeley@rosy";
        #   };
        # };
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
        # cfeeley-laptop = with (collective.peers.hosts.cfeeley-laptop); {
        #   hostname = ipv4.address;
        #   sshUser = "cfeeley";
        #   remoteBuild = true;
        #   fastConnection = true;
        #   autoRollback = true;
        #   magicRollback = true;
        #   profilesOrder = [ "cfeeley" ];
        #   profiles.cfeeley = {
        #     user = "cfeeley";
        #     path = deploy.lib.x86_64-linux.activate.home-manager
        #       self.homeConfigurationsPortable.x86_64-linux."cfeeley@cfeeley-laptop";
        #   };
        # };
      };

      overlays = rec {
        # Helper function to install DMGs
        installApplication = self.overlays."nixpkgs-darwin/installApplication";
        darwin-packages = nixpkgs.lib.composeManyExtensions [
          installApplication

          self.overlays."nixpkgs-darwin/emacs28Macport"
          self.overlays."nixpkgs-darwin/emacs-plus"

          self.overlays."nixpkgs-darwin/macports"
        ];
        linux-packages = nixpkgs.lib.composeManyExtensions [
          self.overlays."nixos-stable/xmonad-config"
          self.overlays."nixos-stable/xsct"
        ];
      };
    })
    //
    # Generate attrs for each system: (formatter.<system>)
    (eachSystem supportedSystems (system: {
      formatter = nixpkgs.legacyPackages.${system}.nixpkgs-fmt;
    })) //
    # Generate attrs for each system: (formatter.<system>)
    {
      packages =
        let
          mkLinuxPackages = system:
            let
              pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.linux-packages ]; config.allowUnfree = true; };
            in
            {
              inherit (pkgs) xmonad-config;
            };

          mkDarwinPackages = system:
            let
              pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.darwin-packages ]; };
            in
            {
              inherit (pkgs) macports amphetamine-enhancer emacs28Macport;

              # - Should most likely not be used as part of a system configuration (use emacs28Macport instead)
              emacs28Macport-noNativeComp = pkgs.emacs28Macport.override { nativeComp = false; };
            } //
            (builtins.mapAttrs (_n: v: nixpkgs.legacyPackages.${system}.callPackage v { inherit (pkgs) installApplication; })
              (flattenTree (rakeLeaves ./darwin/packages)));
        in
        {
          x86_64-linux = mkLinuxPackages "x86_64-darwin";
          aarch64-linux = mkLinuxPackages "x86_64-darwin";
          x86_64-darwin = mkDarwinPackages "x86_64-darwin";
          aarch64-darwin = mkDarwinPackages "x86_64-darwin";
        };
    };

  # Automatic nix.conf settings (accepted automatically when 'accept-flake-config = true')
  nixConfig.extra-experimental-features = "nix-command flakes";
  nixConfig.extra-substituters = [
    "https://cache.nixos.org/"
    "https://nix-community.cachix.org"
    "https://cfeeley.cachix.org"
    "https://devenv.cachix.org"
    "https://cache.iog.io"
    "https://iohk.cachix.org"
  ];
  nixConfig.extra-trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "cfeeley.cachix.org-1:b+RrHsy/4WWys2o6T4YyF66OhdiZUF/R/N46JcS0HJU="
    "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
  ];
}
