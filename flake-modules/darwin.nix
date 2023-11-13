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
        MacBook-Pro = withSystem "aarch64-darwin" (ctx@{ config, inputs', ... }:
          inputs.nix-darwin.lib.darwinSystem {
            # system is not needed with freshly generated hardware-configuration.nix
            # system = "x86_64-linux";  # or set nixpkgs.hostPlatform in a module.
            modules = [
              nixosModules.MacBook-Pro
              inputs.agenix.nixosModules.age
              inputs.home-manager.nixosModules.home-manager
              ../darwin/modules/amphetamine.nix
              ../darwin/modules/tailscale.nix
              ../darwin/modules/input-leap.nix
            ];
          });
        # moduleWithSystem (
        #   perSystem@{ config, pkgs, lib, profiles, collective }:
        #   darwin@{ ... }:
        #   import ../darwin/machines/MacBook-Pro.nix { inherit config pkgs lib profiles collective; }
        # );
      };
      nixosModules.MacBook-Pro =
        (moduleWithSystem (
          perSystem@{ config, pkgs, lib, profiles, collective }:
          darwin@{ ... }:
          let
            roles = import ../darwin/roles { inherit collective profiles; };
          in
          {
            # nixpkgs.hostPlatform = system;

            # lib = import ../lib {
            #   inherit collective;
            #   lib = inputs.digga.lib // inputs.nixos-unstable.lib;
            # };

            imports = [
              ../lib/system
              ../profiles/core/nix-config.nix
              ../profiles/core/system-packages.nix
              ../profiles/secrets.nix
              ../modules/dotfield/guardian.nix

              ../darwin/machines/MacBook-Pro.nix
            ] ++ (
              with roles;
              # workstation ++
              [
                # collective.profiles.hercules-ci-agent
                # profiles.virtualization.nixos-vm-host

                # profiles.emacs # emacs-macport from homebrew
                # profiles.pulseaudio
              ]
            );

            # imports = with roles;
            # workstation ++ [
            #   collective.profiles.hercules-ci-agent
            #   profiles.virtualization.nixos-vm-host

            #   profiles.emacs # emacs-macport from homebrew
            #   profiles.pulseaudio
            # ];
          }
        ));
      # specialArgs.profiles = inputs.digga.lib.rakeLeaves ../profiles;
      specialArgs = {
        rosettaPkgs = import inputs.nixpkgs { system = "x86_64-darwin"; };
      };
    };

    perSystem = { self', system, config, pkgs, ... }: {
      # _module.args.profiles = inputs.digga.lib.rakeLeaves ../profiles;
    };
  };
}
