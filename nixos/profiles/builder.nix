{ config
, lib
, pkgs
, profiles
, suites
, inputs
, primaryUser
, collective
, ...
}:
let
  inherit (collective) peers;
  inherit (config.networking) hostName;
  hostKeys = (lib.our.peers.getHost "MacBook-Pro").keys;
  host = peers.hosts.${hostName};
in
lib.mkMerge [
  {
    # Serve nix store
    nix = {
      sshServe = {
        enable = true;
        protocol = "ssh-ng";
        keys = host.keys;
      };
    };
  }

  {
    nix = {
      distributedBuilds = true;
      buildMachines = [
        {
          hostName = "MacBook-Pro";
          sshUser = config.dotfield.guardian.username;
          protocol = "ssh";
          systems = [ "aarch64-darwin" "x86_64-darwin" ];
          # "8 laptop cores times 1" seems like a reasonable relative speed factor.
          speedFactor = 8;
          supportedFeatures = [ "big-parallel" "kvm" "nixos-test" "benchmark" ];
        }
      ];
    };
  }
]
