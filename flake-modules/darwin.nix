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
in
{
  config = {
    flake = rec {
      darwinConfigurations = {
        MacBook-Pro = withSystem "aarch64-darwin" (ctx@{ self', inputs', config, ... }:
          let
            system = "aarch64-darwin";
            roles = import ../darwin/roles { inherit (self) collective; };
          in
          inputs.darwin.lib.darwinSystem {
            # system is not needed with freshly generated hardware-configuration.nix
            # system = "x86_64-linux";  # or set nixpkgs.hostPlatform in a module.
            specialArgs = rec {
              inherit self' self inputs' inputs;
              pkgs = self.pkgsets.pkgs' "aarch64-darwin";

              # flake-lib = import ../lib {
              #   inherit (self) collective;
              #   lib = inputs.digga.lib // pkgs.lib;
              # };
            };
            modules = [
              inputs.agenix.nixosModules.age # `nixosModules` is correct, even for darwin
              inputs.home-manager.darwinModules.home-manager
              inputs.hercules-ci-agent.darwinModules.agent-service
              inputs.nur.nixosModules.nur

              ../darwin/modules/amphetamine.nix
              ../darwin/modules/tailscale.nix
              ../darwin/modules/input-leap.nix
              ../darwin/modules/hammerspoon.nix
              ../darwin/modules/terminfo.nix

              self.collective.darwinProfiles.core
              self.collective.darwinProfiles.emacs
              self.collective.darwinProfiles.pulseaudio
              self.collective.darwinProfiles.postgres
              self.collective.darwinProfiles.virtualization.nixos-vm-host
              self.collective.profiles.hercules-ci-agent
              self.collective.profiles.core

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
            ] ++ roles.workstation;
          });
      };

      nixosModules.MacBook-Pro =
        (moduleWithSystem (
          perSystem@{ system, config, inputs, pkgs, lib, collective }:
          darwin@{ ... }:
          {
            nixpkgs.config.allowUnfree = true;
            imports = [
              ../lib/system
              ../profiles/core/nix-config.nix
              ../profiles/core/system-packages.nix
              ../profiles/secrets.nix
              ../modules/dotfield/guardian.nix
              ../modules/fup-options.nix
              ../darwin/machines/MacBook-Pro.nix
            ];
          }
        ));
      specialArgs = {
        rosettaPkgs = import inputs.nixpkgs { system = "x86_64-darwin"; };
      };
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
