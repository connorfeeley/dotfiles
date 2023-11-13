# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{
  description = "Dotfield";

  inputs = {
    ##: --- nixpkgs flavours ----------------------------------------------------------
    nixpkgs.follows = "nixos-stable";
    nixpkgs-darwin.follows = "nixos-stable";

    nixos-stable.url = "github:NixOS/nixpkgs/nixos-23.05";
    # nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-unstable-small.url = "github:NixOS/nixpkgs/nixos-unstable-small"; # For docker_24

    nixos-23-05.url = "github:NixOS/nixpkgs/nixos-23.05";

    ##: --- system -------------------------------------------------------------
    home-manager = { url = "github:nix-community/home-manager/release-23.05"; inputs.nixpkgs.follows = "nixpkgs"; };
    darwin = { url = "github:LnL7/nix-darwin"; inputs.nixpkgs.follows = "nixpkgs"; };
    digga = { url = "github:divnix/digga"; inputs.nixpkgs.follows = "nixpkgs"; inputs.home-manager.follows = "home-manager"; inputs.darwin.follows = "darwin"; };
    nixos-wsl = { url = "github:nix-community/NixOS-WSL"; inputs.nixpkgs.follows = "nixpkgs"; };
    agenix = { url = "github:ryantm/agenix"; inputs.nixpkgs.follows = "nixpkgs"; };
    # sops-nix = { url = "github:pogobanane/sops-nix/feat/home-manager-darwin"; inputs.nixpkgs.follows = "nixpkgs"; };
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
    arion = { url = "github:hercules-ci/arion"; inputs.nixpkgs.follows = "nixpkgs"; };
    # nix-serve-ng = { url = "github:aristanetworks/nix-serve-ng"; inputs.nixpkgs.follows = "nixpkgs"; inputs.utils.follows = "flake-utils"; };
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
    hercules-ci-agent = { url = "github:hercules-ci/hercules-ci-agent"; };
    poetry2nix = { url = "github:nix-community/poetry2nix"; inputs = { nixpkgs.follows = "nixpkgs"; }; };

    ##: --- personal packages --------------------------------------------------
    nurpkgs = { url = "github:connorfeeley/nurpkgs"; inputs.nixpkgs.follows = "nixpkgs"; };
    xmonad-config = { url = "git+https://git.sr.ht/~cfeeley/xmonad-config"; };
    ttc-subway-font = { url = "git+ssh://git@git.sr.ht/~cfeeley/ttc-subway-font"; inputs.nixpkgs.follows = "nixpkgs"; }; # Private repo
    nixpkgs-input-leap.url = "git+https://git.sr.ht/~cfeeley/nixpkgs?ref=feat/input-leap";

    ##: --- meta packages ------------------------------------------------------
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; inputs.nixpkgs.follows = "nixpkgs"; };
    darwin-emacs = { url = "github:c4710n/nix-darwin-emacs"; };
    nix-xilinx = { url = "git+https://git.sr.ht/~cfeeley/nix-xilinx"; };
    nixpkgs-doc = { url = "github:aakropotkin/nixpkgs-doc"; inputs.nixpkgs.follows = "nixpkgs"; inputs.utils.follows = "flake-utils"; };

    ##: --- packages -----------------------------------------------------------
    nix-nil = { url = "github:oxalica/nil"; };
    nixd = { url = "github:nix-community/nixd"; };
    nix-init = { url = "github:nix-community/nix-init"; };
    devenv = { url = "github:cachix/devenv/v0.5"; };
    deploy = { url = "github:serokell/deploy-rs"; inputs.nixpkgs.follows = "nixpkgs"; };
    deploy-flake = { url = "github:antifuchs/deploy-flake"; };
    prefmanager = { url = "github:malob/prefmanager"; inputs.nixpkgs.follows = "nixpkgs"; };
    tum-dse-config = { url = "github:TUM-DSE/doctor-cluster-config"; inputs.nixpkgs.follows = "nixpkgs"; inputs.flake-parts.follows = "flake-parts"; };
    neovim-plusultra = { url = "github:jakehamilton/neovim"; };
    nix-search-cli = { url = "github:peterldowns/nix-search-cli"; inputs.nixpkgs.follows = "nixpkgs"; };
    # TODO: currently using package from nur; should change nvidia profile to use this though
    nvidia-patch = { url = "github:arcnmx/nvidia-patch.nix"; inputs.nixpkgs.follows = "nixpkgs"; };

    ##: --- other --------------------------------------------------------------
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    dwarffs.url = "github:edolstra/dwarffs";
    base16-kitty = { url = "github:kdrag0n/base16-kitty"; flake = false; };
    firefox-lepton = { url = "github:black7375/Firefox-UI-Fix"; flake = false; };
    modded-minecraft-servers = { url = "github:mkaito/nixos-modded-minecraft-servers"; inputs.nixpkgs.follows = "nixpkgs"; };
    plasma-manager = { url = "github:pjones/plasma-manager"; inputs.nixpkgs.follows = "nixpkgs"; };
    mmdoc = { url = "github:ryantm/mmdoc"; inputs.nixpkgs.follows = "nixpkgs"; };
    nixpkgs-update = { url = "github:ryantm/nixpkgs-update"; inputs.nixpkgs.follows = "nixpkgs"; inputs.mmdoc.follows = "mmdoc"; };
    visionfive-nix = { url = "github:connorfeeley/visionfive-nix"; inputs.nixpkgs.follows = "nixos-23-05"; };
  };

  outputs = inputs@{ flake-parts, ... }:
    # { self
    # , nixpkgs
    # , nixos-stable
    # , nixpkgs-darwin
    # , nixos-unstable
    # , nixos-21-11
    # , home-manager
    # , darwin
    # , digga
    # , agenix
    # , nixos-hardware
    # , nix-alien
    # , nix-autobahn
    # , envfs
    # , flake-utils
    # , flake-parts
    # , nix-darwin
    # , nur
    # , nixos-generators
    # , nvfetcher
    # , arion
    # , nix-serve-ng
    # , nixago
    # , nixos-vscode-server
    # , mach-nix
    # , gitignore
    # , nix-colors
    # , deadnix
    # , comma
    # , rnix-lsp
    # , nurpkgs
    # , xmonad-config
    # , ttc-subway-font
    # , nixpkgs-input-leap
    # , emacs-overlay
    # , darwin-emacs
    # , nix-xilinx
    # , nixpkgs-doc
    # , nix-nil
    # , nixd
    # , nix-init
    # , devenv
    # , deploy
    # , deploy-flake
    # , prefmanager
    # , tum-dse-config
    # , neovim-plusultra
    # , nix-search-cli
    # , flake-compat
    # , dwarffs
    # , base16-kitty
    # , firefox-lepton
    # , modded-minecraft-servers
    # , plasma-manager
    # , ...
    # }@inputs:
    (flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [
        # Import this repo's modules.
        ./flake-module.nix
      ];
      flake = {
        # Put your original flake attributes here.
      };
      perSystem = { config, pkgs, inputs', ... }:
      let
        installApplication = pkgs.darwin.apple_sdk_11_0.callPackage ./packages/darwin/installApplication.nix { };
      in {
        packages.amphetamine-enhancer = pkgs.callPackage ./darwin/packages/amphetamine-enhancer.nix { inherit installApplication; };
        packages.hammerspoon = pkgs.callPackage ./darwin/packages/hammerspoon.nix { };
        packages.mints = pkgs.callPackage ./darwin/packages/mints { };
        packages.better-display = pkgs.callPackage ./darwin/packages/better-display.nix { inherit installApplication; };
      };
    });
    # (digga.lib.mkFlake {
    #   inherit self inputs supportedSystems;

    #   channelsConfig = {
    #     allowUnfree = true;
    #     allowUnsupportedSystem = false;

    #     allowBroken = false;

    #     permittedInsecurePackages = [
    #       "nodejs-16.20.2"
    #       "nodejs-14.21.3"
    #       "openssl-1.1.1v"
    #     ];
    #   };

    #   ###
    #   ### Overlay & Channel Configuration
    #   ###
    #   channels = {
    #     nixos-stable = {
    #       inherit overlays;
    #       imports = commonImports
    #       ++ [ (digga.lib.importOverlays ./overlays/stable) ];
    #     };
    #     nixpkgs-darwin = {
    #       imports = commonImports ++ [
    #         (digga.lib.importOverlays ./overlays/nixpkgs-darwin)
    #         (digga.lib.importOverlays ./overlays/stable)
    #       ];
    #       overlays = overlays ++ [
    #         (final: _prev: {
    #           amphetamine-enhancer =
    #             self.packages.${final.system}.amphetamine-enhancer;
    #           mints = self.packages.${final.system}.mints;
    #           hammerspoon = self.packages.${final.system}.hammerspoon;
    #           native-youtube = self.packages.${final.system}.native-youtube;
    #           better-display = self.packages.${final.system}.better-display;
    #         })
    #       ];
    #     };
    #     nixos-unstable = {
    #       inherit overlays;

    #       imports = commonImports
    #       ++ [ (digga.lib.importOverlays ./overlays/nixos-unstable) ];
    #     };
    #   };

    #   sharedOverlays = [
    #     (_final: prev: {
    #       __dontExport = true;
    #       inherit inputs;
    #       lib = prev.lib.extend (_lfinal: _lprev: { our = self.lib; });
    #     })
    #   ];

    #   ###
    #   ### Other attributes
    #   ###
    #   lib = import ./lib {
    #     inherit collective;
    #     lib = digga.lib // nixos-unstable.lib;
    #   };

    #   nixos = import ./nixos collective;
    #   darwin = import ./darwin collective;
    #   home = import ./home collective;

    #   devshell = ./shell;

    #   homeConfigurations = digga.lib.mergeAny
    #     (digga.lib.mkHomeConfigurations self.darwinConfigurations)
    #     (digga.lib.mkHomeConfigurations self.nixosConfigurations);

    #   deploy = import ./deploy.nix { inherit self collective deploy digga; };

    #   overlays = rec {
    #     # Helper function to install DMGs
    #     installApplication = self.overlays."nixpkgs-darwin/installApplication";
    #     darwin-packages = nixpkgs.lib.composeManyExtensions [
    #       installApplication

    #       self.overlays."nixpkgs-darwin/macports"
    #       self.overlays."nixpkgs-darwin/emacs-plus"
    #     ];
    #     linux-packages = nixpkgs.lib.composeManyExtensions [
    #       # FIXME(darwin): causes 'nix flake show' to error
    #       # self.overlays."nixos-stable/xmonad-config"

    #       self.overlays."nixos-stable/xsct"
    #       self.overlays."nixos-stable/mdio-tools"
    #       self.overlays."nixos-stable/aranet4"
    #       # self.overlays."nixos-stable/fildem-global-menu"
    #     ];
    #   };
    # }) //
    # {
    #   images = {
    #     # visionfive-cross = self.nixosConfigurations.visionfive-cross.config.system.build.sdImage;
    #     # visionfive-native = self.nixosConfigurations.visionfive-native.config.system.build.sdImage;

    #     visionfive2-cross = self.nixosConfigurations.visionfive2.config.system.build.sdImage;
    #     # visionfive2-native = self.nixosConfigurations.visionfive2-native.config.system.build.sdImage;
    #   };
    # } //
    # # Generate attrs for each system: (formatter.<system>)
    # (eachSystem supportedSystems
    #   (system: { formatter = nixpkgs.legacyPackages.${system}.nixpkgs-fmt; }))
    # //
    # # Generate attrs for each system: (formatter.<system>)
    # {
    #   packages =
    #     let
    #       mkLinuxPackages = system:
    #         let
    #           pkgs = import nixpkgs {
    #             inherit system;
    #             overlays = [ self.overlays.linux-packages ];
    #             config.allowUnfree = true;
    #           };
    #         in
    #         {
    #           # FIXME(darwin): causes 'nix flake show' to error
    #           inherit (pkgs) xsct mdio-tools aranet4;
    #         };

    #       mkDarwinPackages = system:
    #         let
    #           pkgs = import nixpkgs {
    #             inherit system;
    #             overlays = [ self.overlays.darwin-packages ];
    #             config.allowUnfree = true;
    #           };
    #         in
    #         {
    #           inherit (pkgs)
    #             macports amphetamine-enhancer mints hammerspoon native-youtube
    #             better-display;
    #         } // (builtins.mapAttrs
    #           (_n: v: pkgs.callPackage v { inherit (pkgs) installApplication; })
    #           (flattenTree (rakeLeaves ./darwin/packages)));
    #     in
    #     {
    #       x86_64-linux = mkLinuxPackages "x86_64-linux";
    #       aarch64-linux = mkLinuxPackages "aarch64-linux";
    #       x86_64-darwin = mkDarwinPackages "x86_64-darwin";
    #       aarch64-darwin = mkDarwinPackages "aarch64-darwin";
    #     };

    #   templates =
    #     let
    #       poetry2nix = {
    #         path = ./templates/poetry2nix;
    #         description = "poetry2nix template";
    #         welcomeText =
    #           "Set project name: sed --in-place 's/poetry2nixTemplate/<project-name>/g' flake.nix";
    #       };
    #     in
    #     {
    #       inherit poetry2nix;
    #       default = poetry2nix;
    #     };
    # };

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
