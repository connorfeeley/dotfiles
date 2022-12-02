{ config
, lib
, primaryUser
, collective
, ...
}:
let
  inherit (config.networking) hostName;
in
lib.mkMerge [
  {
    # Serve nix store
    nix = {
      sshServe = {
        enable = true;
        write = true;
        protocol = "ssh-ng";
        # A list of SSH public keys allowed to access the binary cache via SSH.
        keys = primaryUser.authorizedKeys;
      };
    };
  }

  {
    nix = {
      distributedBuilds = true;
      buildMachines = [
        {
          hostName = "MacBook-Pro";
          protocol = "ssh";
          systems = [ "aarch64-darwin" "x86_64-darwin" ];
          # Base64-encoded ed25519 public host key of builder:
          #   base64 -w0 /etc/ssh/ssh_host_ed25519_key.pub
          publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUtWcnlic0pyQU5yTytGc2xTNU1GcnBNVHRjMkVCZ1hyaUVvRjRzcnFRcnggCg==";
          sshUser = config.dotfield.guardian.username;
          # "8 laptop cores times 1" seems like a reasonable relative speed factor.
          speedFactor = 8;
          supportedFeatures = [ "big-parallel" "kvm" "nixos-test" "benchmark" ];
        }
      ];
    };
  }
]
