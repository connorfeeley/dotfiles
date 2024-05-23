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
      nixosConfigurations = {
        workstation = withSystem "x86_64-linux" (ctx@{ self', inputs', config, ... }:
          let
            system = "x86_64-linux";
          in
          inputs.nixpkgs.lib.nixosSystem {
            # system is not needed with freshly generated hardware-configuration.nix
            # system = "x86_64-linux";  # or set nixpkgs.hostPlatform in a module.
            specialArgs = rec {
              inherit self' self inputs' inputs;
              pkgs = self.pkgsets.pkgs' "x86_64-linux";
              primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

              # flake-lib = import ../lib {
              #   inherit collective;
              #   lib = inputs.digga.lib // pkgs.lib;
              # };
            };
            modules = [
              inputs.agenix.nixosModules.age
              inputs.home-manager.nixosModules.home-manager
              inputs.nur.nixosModules.nur
              inputs.nixos-vscode-server.nixosModules.default
              inputs.nurpkgs.nixosModules.mellanox

              collective.profiles.nixos.core
              collective.profiles.nixos.xorg
              collective.profiles.nixos.wine
              # collective.profiles.nixos.emacs
              # collective.profiles.nixos.pulseaudio
              # collective.profiles.nixos.virtualization.nixos-vm-host
              collective.profiles.global.core

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
            ]
            ++ collective.roles.nixos.graphical
            ++ collective.roles.nixos.server
            ++ collective.roles.nixos.tangible
            ++ collective.roles.nixos.virt
            ++ collective.roles.nixos.fpgadev
            ++ (with collective.profiles.nixos; [
              core
              hardware.amd
              nvidia
              virtualisation.vm-variant
              hidpi
              smart

              builder
              # binary-cache

              mail

              workstations.flatpak
              games

              desktop.common
              desktop.input-leap
              login.gdm
              xorg
              # hm-xmonad
              kde
              gnome-desktop
              xfce
              # pantheon

              grafana

              # virtualisation.arion

              # hardware.machine-check-exception
            ]);
          });
      };

      nixosModules.workstation =
        (moduleWithSystem (
          perSystem@{ system, config, inputs, pkgs, lib, self }:
          nixos@{ ... }:
          {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.config.cudaSupport = true;

            imports = [
              ../lib/system
              ../profiles/core/nix-config.nix
              ../profiles/core/system-packages.nix
              ../profiles/secrets.nix
              ../modules/dotfield/guardian.nix
              ../nixos/machines/workstation


              ../modules/fup-options.nix
              ../modules/nixos-vm

              collective.modules.nixos.boot-unlock
              collective.modules.nixos.input-leap
              collective.modules.nixos.substituter
              collective.modules.nixos.audio
            ];
          }
        ));

      nixosModules.media-dl =
        (moduleWithSystem (
          perSystem@{ system, config, inputs, pkgs, lib, self }:
          nixos@{ ... }:
          {
            nixpkgs.config.allowUnfree = true;
            imports = [
              # ../lib/system
              # ../profiles/core/nix-config.nix
              # ../profiles/core/system-packages.nix
              ../profiles/secrets.nix
              # ../modules/dotfield/guardian.nix
              collective.modules.nixos.media-dl
              ({ config, ... }:
                let inherit (config.lib.dotfield.secrets) secretsDir secretsGroup;
                in {
                  age.secrets = {
                    openvpn-auth-file = {
                      file = "${secretsDir}/openvpn-auth-file.txt.age";
                      group = secretsGroup;
                    };
                  };
                })
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
