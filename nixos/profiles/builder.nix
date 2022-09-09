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
          sshUser = config.user.name;
          # Base64-encoded ed25519 public host key
          publicHostKey = lib.head hostKeys;
          # "8 laptop cores times 1.5 (versus times 1 for an x64 laptop)" seems like
          # a reasonable relative speed factor.
          speedFactor = 8 * 1.5;
          supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        }
      ];
    };
  }
]
