# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self, lib, inputs, flake-parts-lib, moduleWithSystem, withSystem, ... }:

let
  collective = {
    peers = import ../ops/metadata/peers.nix;

    modules = {
      global = inputs.digga.lib.rakeLeaves ../modules;
      nixos = inputs.digga.lib.rakeLeaves ../nixos/modules;
      darwin = inputs.digga.lib.rakeLeaves ../darwin/modules;
    };

    profiles = {
      global = inputs.digga.lib.rakeLeaves ../profiles;
      nixos = inputs.digga.lib.rakeLeaves ../nixos/profiles;
      darwin = inputs.digga.lib.rakeLeaves ../darwin/profiles;
    };

    roles = {
      nixos = import ../nixos/roles { inherit (self) collective; };
      darwin = import ../darwin/roles { inherit (self) collective; };
      home = import ../home/roles { inherit (self) collective; };
    };

    machines = {
      nixos = inputs.digga.lib.rakeLeaves ../nixos/machines;
      darwin = inputs.digga.lib.rakeLeaves ../darwin/machines;
    };

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

      specialArgs = {
        rosettaPkgs = import inputs.nixpkgs { system = "x86_64-darwin"; };
      };
    };
  };
}
