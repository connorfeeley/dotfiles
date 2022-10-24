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
      sshServe.enable = true;
      sshServe.keys = host.keys;
    };
  }

  {
    nix = {
      distributedBuilds = true;
      buildMachines = [
        {
          hostName = "MacBook-Pro";
          systems = [ "aarch64-darwin" "x86_64-darwin" ];
          sshUser = config.dotfield.guardian.username;
          # "8 laptop cores times 1" seems like a reasonable relative speed factor.
          speedFactor = 8;
          supportedFeatures = [ ];
        }
      ];
    };
  }
]
