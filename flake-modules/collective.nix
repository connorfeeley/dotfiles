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

  collective = {
    modules = builtins.attrValues (inputs.digga.lib.flattenTree (inputs.digga.lib.rakeLeaves ../modules));
    peers = import ../ops/metadata/peers.nix;
    profiles = inputs.digga.lib.rakeLeaves ../profiles;
    darwinProfiles = inputs.digga.lib.rakeLeaves ../darwin/profiles;
    hmArgs = {
      profiles = inputs.digga.lib.rakeLeaves ../home/profiles;
      roles = import ../home/roles { inherit (self) collective; };
      modules = builtins.attrValues (inputs.digga.lib.flattenTree (inputs.digga.lib.rakeLeaves ../home/modules));
    };
  };
in
{
  config = {
    flake = {
      inherit collective;

      flake-lib = import ../lib { inherit collective; lib = inputs.nixpkgs.lib; };
    };
  };
}
