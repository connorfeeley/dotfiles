# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{
  outputs = inputs@{ flake-parts, ... }:
    (flake-parts.lib.mkFlake { inherit inputs; } ({ config, flake-parts-lib, getSystem, inputs, lib, options, ... }: {
      debug = true;
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay

        # Import this repo's modules.
        ./flake-modules
        inputs.devshell.flakeModule
      ];

      perSystem = { config, pkgs, inputs', system, ... }:
        let
          mkPackages =  pkgs.lib.makeScope pkgs.newScope (self:
              let
                commonPackages = import ./packages/common { inherit pkgs; inherit (pkgs) nodePackages; inherit (pkgs) callPackage; };
                pythonPackages = pkgs.lib.recurseIntoAttrs (pkgs.python3.pkgs.callPackage ./packages/python { });

                installApplication = pkgs.darwin.apple_sdk_11_0.callPackage ./packages/darwin/installApplication.nix { };
                darwinPackages = builtins.mapAttrs
                  (_n: v: pkgs.callPackage v { inherit installApplication; })
                  (inputs.digga.lib.flattenTree (inputs.digga.lib.rakeLeaves ./darwin/packages));
              in
              (filterSystem (commonPackages // pythonPackages // darwinPackages)));

          # Only packages available on the system.
          filterSystem = attrs: pkgs.lib.filterAttrs (n: v: pkgs.lib.meta.availableOn { inherit system; } v) attrs;

          # Filter out attributes from newScope.
          filterPackages = attrs: pkgs.lib.filterAttrs
            (n: v: ! pkgs.lib.elem n [
              "packages"
              "callPackage"
              "default"
              "newScope"
              "override"
              "overrideScope"
              "overrideScope'"
              "recurseForDerivations"
              "overrideDerivation"
            ])
            attrs;
        in
        {
          packages = filterPackages mkPackages;

          devshells.default = ./shell/dotfield.nix;
        };
    }));

  # Automatic nix.conf settings (accepted automatically when 'accept-flake-config = true')
  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
      "https://cfeeley.cachix.org"
      "https://cuda-maintainers.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cfeeley.cachix.org-1:b+RrHsy/4WWys2o6T4YyF66OhdiZUF/R/N46JcS0HJU="
      "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
    ];
  };

  description = "My life in Nix.";

  inputs = {
    ##: --- nixpkgs flavours ----------------------------------------------------------
    nixpkgs.follows = "nixos-stable";
    nixpkgs-darwin.follows = "nixos-stable";

    nixos-stable.url = "github:NixOS/nixpkgs/nixpkgs-23.11-darwin";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-unstable-small.url = "github:NixOS/nixpkgs/nixos-unstable-small"; # For docker_24

    nixos-23-05.url = "github:NixOS/nixpkgs/nixos-23.05";

    ##: --- system -------------------------------------------------------------
    home-manager = { url = "github:nix-community/home-manager/release-23.11"; inputs.nixpkgs.follows = "nixpkgs"; };
    darwin = { url = "github:LnL7/nix-darwin"; inputs.nixpkgs.follows = "nixpkgs"; };
    digga = { url = "github:divnix/digga"; inputs.nixpkgs.follows = "nixpkgs"; inputs.home-manager.follows = "home-manager"; inputs.darwin.follows = "darwin"; };
    nixos-wsl = { url = "github:nix-community/NixOS-WSL"; inputs.nixpkgs.follows = "nixpkgs"; };
    agenix = { url = "github:ryantm/agenix"; inputs.nixpkgs.follows = "nixpkgs"; };
    sops-nix = { url = "github:pogobanane/sops-nix/feat/home-manager-darwin"; inputs.nixpkgs.follows = "nixpkgs"; };
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # : ~~~ FHS compat ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    nix-alien = { url = "github:thiagokokada/nix-alien"; inputs.nixpkgs.follows = "nixpkgs"; };
    nix-autobahn = { url = "github:Lassulus/nix-autobahn"; inputs.nixpkgs.follows = "nixpkgs"; };
    envfs = { url = "github:Mic92/envfs"; inputs.nixpkgs.follows = "nixpkgs"; };

    ##: --- utilities ----------------------------------------------------------
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts = { url = "github:hercules-ci/flake-parts"; };
    devshell = { url = "github:numtide/devshell"; inputs.nixpkgs.follows = "nixpkgs"; };

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
    # prefmanager = { url = "github:malob/prefmanager"; inputs.nixpkgs.follows = "nixpkgs"; };
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
  };
}
