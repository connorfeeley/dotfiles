# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self, lib, collective, inputs, flake-parts-lib, moduleWithSystem, withSystem, ... }:

let
  inherit (flake-parts-lib)
    mkPerSystemOption;
  inherit (lib)
    mkOption
    mkPackageOption
    types;

  # FIXME: split this to shared/nixos/darwin-specific
  overlays = [
    inputs.agenix.overlays.default
    inputs.emacs-overlay.overlay
    inputs.gitignore.overlay
    inputs.nur.overlay
    inputs.nvfetcher.overlays.default

    inputs.nix-xilinx.overlay

    inputs.nixpkgs-doc.overlays.default # add info outputs to nixpkgs.htmlDocs.nixpkgsManual

    # Personal overlay
    inputs.nurpkgs.overlays.default

    (import ../packages/fonts/common)

    (final: prev:
      let
        packagesFrom = inputAttr:
          inputAttr.packages.${final.system};
      in
      {
        inherit (packagesFrom self.packages) emacs-plus;
        inherit (inputs.nixos-unstable.legacyPackages.${final.system}) emacs29-macport;
        inherit (packagesFrom inputs.devenv) devenv;
        inherit (packagesFrom inputs.deploy) deploy-rs;
        inherit (packagesFrom inputs.deploy-flake) deploy-flake;
        inherit (packagesFrom inputs.prefmanager) prefmanager;
        inherit (packagesFrom inputs.nix-nil) nil;
        inherit (packagesFrom inputs.nix-alien) nix-alien;
        inherit (packagesFrom inputs.nix-alien) nix-index-update;
        inherit (packagesFrom inputs.nix-autobahn) nix-autobahn;
        inherit (packagesFrom inputs.mmdoc) mmdoc;
        inherit (packagesFrom inputs.nixpkgs-update) nixpkgs-update nixpkgs-update-doc;
        inherit (packagesFrom inputs.nix-search-cli) nix-search;
        inherit (packagesFrom inputs.nixd) nixd;

        inherit (packagesFrom inputs.xmonad-config) xmonad-config;
        inherit (packagesFrom inputs.ttc-subway-font)
          ttc-subway bloor-yonge-font;

        inherit (packagesFrom inputs.nixpkgs-input-leap) input-leap;

        nix-init = inputs.nix-init.packages.${final.system}.default;
        emacsGitDarwin =
          inputs.darwin-emacs.packages.${final.system}.default;
        neovim-plusultra =
          inputs.neovim-plusultra.packages.${final.system}.neovim;

        inherit (inputs.nixos-unstable-small.legacyPackages.${final.system}) docker_24;
        docker = inputs.nixos-unstable-small.legacyPackages.${final.system}.docker_24;
        docker-compose = inputs.nixos-unstable-small.legacyPackages.${final.system}.docker-compose;

        # Broken on nixos-23.05
        inherit (inputs.nixos-unstable.legacyPackages.${final.system}) github-copilot-cli;

        inherit (packagesFrom self.packages) amphetamine-enhancer;
        inherit (packagesFrom self.packages) hammerspoon;
        inherit (packagesFrom self.packages) mints;

        lib = prev.lib.extend (_lfinal: _lprev: {
          our = import ../lib { inherit collective; lib = inputs.digga.lib // prev.lib; };
        });
      })
    (import ../packages/common)
    (import ../packages/sources)
    inputs.nur.overlay
  ];

  commonImports = [
  ];
in
{
  config = {
    flake.overlays.cfeeley-overlay = overlays;
    flake.pkgsets = {
      pkgs' = system: (import inputs.nixpkgs {
        inherit system;
        inherit overlays;
        config = {
          nixpkgs.config.allowUnfree = true;
          allowUnfree = true;
          allowUnsupportedSystem = true;

          permittedInsecurePackages = [
            "nodejs-16.20.2"
          ];
        };
      });
    };

    perSystem = { self', system, config, pkgs, ... }:
      let pkgs' = self.pkgsets.pkgs' system;
      in {
        _module.args = {
          pkgs = pkgs';
          lib = pkgs'.lib // { hm = inputs.home-manager.lib; };
        };
      };
  };
}
