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
      nixosConfigurations = {
        workstation = withSystem "x86_64-linux" (ctx@{ self', inputs', config, ... }:
          let
            system = "x86_64-linux";
            roles = import ../nixos/roles { inherit (self) collective; };
          in
          inputs.nixpkgs.lib.nixosSystem {
            # system is not needed with freshly generated hardware-configuration.nix
            # system = "x86_64-linux";  # or set nixpkgs.hostPlatform in a module.
            specialArgs = rec {
              inherit self' self inputs' inputs;
              pkgs = self.pkgsets.pkgs' "x86_64-linux";
              primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

              # flake-lib = import ../lib {
              #   inherit (self) collective;
              #   lib = inputs.digga.lib // pkgs.lib;
              # };
            };
            modules = [
              inputs.agenix.nixosModules.age
              inputs.home-manager.nixosModules.home-manager
              inputs.hercules-ci-agent.nixosModules.agent-service
              inputs.nur.nixosModules.nur
              inputs.nixos-vscode-server.nixosModules.default
              inputs.nurpkgs.nixosModules.mellanox

              self.collective.nixosProfiles.core
              self.collective.nixosProfiles.xorg
              # self.collective.nixosProfiles.emacs
              # self.collective.nixosProfiles.pulseaudio
              # self.collective.nixosProfiles.virtualization.nixos-vm-host
              self.collective.profiles.hercules-ci-agent
              self.collective.profiles.core

              nixosModules.workstation
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
            ] ++ roles.graphical ++ roles.server ++ roles.tangible ++ roles.virt ++ roles.fpgadev ++ (with self.collective.nixosProfiles; [
              core
              hardware.amd
              nvidia
              virtualisation.vm-variant
              smart

              builder
              # binary-cache

              mail

              workstations.flatpak
              games

              desktop.common
              # desktop.input-leap
              login.gdm
              xorg
              # hm-xmonad
              kde
              gnome-desktop
              xfce
              # pantheon

              grafana

              # virtualisation.arion

            ]);
          });
      };

      nixosModules.workstation =
        (moduleWithSystem (
          perSystem@{ system, config, inputs, pkgs, lib, collective }:
          nixos@{ ... }:
          {
            nixpkgs.config.allowUnfree = true;
            imports = [
              ../lib/system
              ../profiles/core/nix-config.nix
              ../profiles/core/system-packages.nix
              ../profiles/secrets.nix
              ../modules/dotfield/guardian.nix
              ../nixos/machines/workstation


              ../modules/fup-options.nix
              ../modules/nixos-vm
              ../nixos/modules/boot-unlock.nix
              # ../nixos/modules/input-leap.nix
              ../nixos/modules/substituter.nix
            ];
          }
        ));
      specialArgs = {
        # rosettaPkgs = import inputs.nixpkgs { system = "x86_64-darwin"; };
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
