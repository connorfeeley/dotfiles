# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self, inputs, ... }:

{
  config.flake = {
    homeConfigurations."cfeeley@proxmox-builder" =
      inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = self.pkgsets.pkgs' "x86_64-linux";
        modules = [
          {
            home.username = "cfeeley";
            home.homeDirectory = "/home/cfeeley";
            home.stateVersion = "23.11";
          }
        ];
      };

    deploy = import ../deploy.nix {
      inherit self;
      inherit (self) collective;
      inherit (inputs) deploy digga;
    };
  };
}
