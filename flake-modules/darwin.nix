# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self, lib, inputs, flake-parts-lib, moduleWithSystem, withSystem, ... }:

let
  inherit (flake-parts-lib)
    mkPerSystemOption;
  inherit (lib)
    mkOption
    mkPackageOption
    types;

  inherit (self) collective;
in
{
  config = {
    flake = rec {
      darwinConfigurations = {
        MacBook-Pro = withSystem "aarch64-darwin" (ctx@{ self', inputs', config, ... }:
          let
            system = "aarch64-darwin";
          in
          inputs.darwin.lib.darwinSystem {
            # system is not needed with freshly generated hardware-configuration.nix
            # system = "x86_64-linux";  # or set nixpkgs.hostPlatform in a module.
            specialArgs = rec {
              inherit self' self inputs' inputs;
              pkgs = self.pkgsets.pkgs' "aarch64-darwin";

              # flake-lib = import ../lib {
              #   inherit collective;
              #   lib = inputs.digga.lib // pkgs.lib;
              # };
            };
            modules = [
              inputs.agenix.nixosModules.age # `nixosModules` is correct, even for darwin
              inputs.home-manager.darwinModules.home-manager
              inputs.nur.nixosModules.nur

              collective.modules.darwin.amphetamine
              collective.modules.darwin.tailscale
              collective.modules.darwin.input-leap
              collective.modules.darwin.hammerspoon
              collective.modules.darwin.terminfo

              collective.profiles.darwin.core
              collective.profiles.darwin.emacs
              collective.profiles.darwin.pulseaudio
              collective.profiles.darwin.postgres
              collective.profiles.darwin.virtualization.nixos-vm-host
              collective.profiles.darwin.networking.tools
              collective.profiles.global.core

              nixosModules.MacBook-Pro
              ({ ... }: {
                nixpkgs.hostPlatform = system;
                nixpkgs.config.allowUnfree = true;
                home-manager.extraSpecialArgs = rec {
                  inherit self self' inputs';

                  pkgs = self.pkgsets.pkgs' system;

                  flake-lib = self.flake-lib;

                  imports = [
                    self.inputs.nur.hmModules.nur
                  ];
                };
              })
            ] ++ collective.roles.darwin.workstation;
          });
      };

      nixosModules.MacBook-Pro =
        (moduleWithSystem (
          perSystem@{ system, config, inputs, pkgs, lib, self }:
          darwin@{ ... }:
          {
            nixpkgs.config.allowUnfree = true;
            imports = [
              ../lib/system

              collective.profiles.global.core
              collective.profiles.global.secrets

              collective.modules.global.nix-config-defaults
              collective.modules.global.dotfield.guardian
              collective.modules.global.fup-options
              collective.modules.global.nix.caches

              # Attic
              collective.modules.darwin.attic
              collective.modules.global.cache

              collective.machines.darwin.MacBook-Pro
            ];
          }
        ));
    };
    perSystem = { self, self', system, config, pkgs, collective, ... }:
      let
        pkgs' = import inputs.nixpkgs {
          inherit system;
          inherit (self.overlays) cfeeley-overlay;
          config = {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = self.overlays.cfeeley-overlay;
          };
        };
      in
      {
        _module.args = {
          flake-lib = import ../lib {
            inherit collective;
            lib = inputs.digga.lib // pkgs'.lib;
          };
        };
      };
  };
}
