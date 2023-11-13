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
        MacBook-Pro = withSystem "aarch64-darwin" (ctx@{ self', inputs', config, profiles, ... }:
          let
            roles = import ../darwin/roles { inherit (self) collective; inherit profiles; };
          in
          inputs.darwin.lib.darwinSystem {
            # system is not needed with freshly generated hardware-configuration.nix
            # system = "x86_64-linux";  # or set nixpkgs.hostPlatform in a module.
            specialArgs = {
              inherit self' self inputs';
            };
            modules = [
              inputs.agenix.nixosModules.age
              inputs.home-manager.darwinModules.home-manager
              ../darwin/modules/amphetamine.nix
              ../darwin/modules/tailscale.nix
              ../darwin/modules/input-leap.nix
              ../darwin/modules/hammerspoon.nix

              nixosModules.MacBook-Pro
              profiles.emacs
              profiles.pulseaudio
              profiles.virtualization.nixos-vm-host
              self.collective.profiles.hercules-ci-agent
            ] ++ roles.workstation;
          });
      };
      nixosModules.MacBook-Pro =
        (moduleWithSystem (
          perSystem@{ config, inputs, pkgs, lib, collective }:
          darwin@{ ... }:
          {
            nixpkgs.hostPlatform = "aarch64-darwin";

            imports = [
              ../lib/system
              ../profiles/core/nix-config.nix
              ../profiles/core/system-packages.nix
              ../profiles/secrets.nix
              ../modules/dotfield/guardian.nix
              ../darwin/machines/MacBook-Pro.nix
            ] ++ (
              # with roles;
              # workstation ++
              [
                # collective.profiles.hercules-ci-agent
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
      # specialArgs = {
      #   rosettaPkgs = import inputs.nixpkgs { system = "x86_64-darwin"; };
      # };
    };

    # perSystem = { self', system, config, pkgs, ... }: {
    #   _module.args.profiles =
    #     let
    #       rakeLeaves =
    #         /*
    #          *
    #            Synopsis: rakeLeaves _path_

    #            Recursively collect the nix files of _path_ into attrs.

    #            Output Format:
    #            An attribute set where all `.nix` files and directories with `default.nix` in them
    #            are mapped to keys that are either the file with .nix stripped or the folder name.
    #            All other directories are recursed further into nested attribute sets with the same format.

    #            Example file structure:
    #            ```
    #            ./core/default.nix
    #            ./base.nix
    #            ./main/dev.nix
    #            ./main/os/default.nix
    #            ```

    #            Example output:
    #            ```
    #            {
    #            core = ./core;
    #            base = base.nix;
    #            main = {
    #            dev = ./main/dev.nix;
    #            os = ./main/os;
    #            };
    #            }
    #            ```
    #          *
    #          */
    #         dirPath:
    #         let
    #           seive = file: type:
    #             # Only rake `.nix` files or directories
    #             (type == "regular" && lib.hasSuffix ".nix" file) || (type == "directory");

    #           collect = file: type: {
    #             name = lib.removeSuffix ".nix" file;
    #             value =
    #               let
    #                 path = dirPath + "/${file}";
    #               in
    #               if
    #                 (type == "regular")
    #                 || (type == "directory" && builtins.pathExists (path + "/default.nix"))
    #               then path
    #               # recurse on directories that don't contain a `default.nix`
    #               else rakeLeaves path;
    #           };

    #           files = lib.filterAttrs seive (builtins.readDir dirPath);
    #         in
    #         lib.filterAttrs (n: v: v != { }) (lib.mapAttrs' collect files);
    #     in
    #     rakeLeaves ../darwin/profiles;
    # };
  };
}
