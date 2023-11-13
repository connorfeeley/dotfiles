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
    flake = {
      collective = {
        modules = inputs.digga.lib.importExportableModules ../modules;
        peers = import ../ops/metadata/peers.nix;
        profiles = inputs.digga.lib.rakeLeaves ../profiles;
        darwinProfiles = inputs.digga.lib.rakeLeaves ../darwin/profiles;
      };
    };

    perSystem = { self', system, config, pkgs, ... }: { };
  };
}
