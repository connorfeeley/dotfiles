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
      user = "cfeeley";
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

    # Deploy to 'cfeeley-laptop':
    # - Recommended: deploy .#cfeeley-laptop -- --print-build-logs
    # If there are nix eval errors, then we can tell 'deploy' to skip the flake checks:
    # - Not recommended: deploy --skip-checks .#cfeeley-laptop -- --print-build-logs
    cfeeley-laptop = with (collective.peers.hosts.cfeeley-laptop); {
      hostname = ipv4.address;
      sshUser = "cfeeley";
      fastConnection = true;
      autoRollback = true;
      magicRollback = true;
      profilesOrder = [ "cfeeley" ];
      profiles.cfeeley = {
        user = "cfeeley";
        path = deploy.lib.x86_64-linux.activate.home-manager
          self.homeConfigurationsPortable.x86_64-linux."cfeeley@cfeeley-laptop";
      };
    };
  };
}
