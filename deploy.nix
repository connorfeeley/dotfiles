# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self, collective, deploy, digga }:

let
  # mkDeploy takes a hostname and an architecture and returns the deploy configuration for that host.
  mkDeploy = hostname: arch: {
    inherit hostname;
    sshUser = "cfeeley";
    remoteBuild = true;
    fastConnection = true;
    autoRollback = true;
    magicRollback = true;
    profiles.system = {
      user = "root";
      path = deploy.lib.${arch}.activate.nixos
        self.nixosConfigurations.${hostname};
    };
    profiles.cfeeley = {
      user = "cfeeley";
      path = deploy.lib.${arch}.activate.home-manager
        self.homeConfigurations."cfeeley@${hostname}";
    };
  };
in
{
  nodes = digga.lib.mkDeployNodes { } {
    workstation = mkDeploy "workstation" "x86_64-linux";
    workstation-wsl = mkDeploy "workstation-wsl" "x86_64-linux";
    rosy = mkDeploy "rosy" "aarch64-linux";
    h8tsner = mkDeploy "h8tsner" "x86_64-linux";
    proxmox-builder = mkDeploy "proxmox-builder" "x86_64-linux";

    # cfeeley-laptop deploy is disabled: depends on
    # self.homeConfigurationsPortable.x86_64-linux."cfeeley@cfeeley-laptop",
    # which is no longer produced by any flake-module. Re-enable by defining
    # that output (digga used to provide homePortable wiring) before
    # uncommenting. The user/host metadata still lives in home/default.nix
    # and ops/metadata/hosts.toml.
  };
}
