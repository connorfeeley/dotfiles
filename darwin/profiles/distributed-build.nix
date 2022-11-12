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
  host = peers.hosts.${hostName};
in
lib.mkMerge [
  {
    nix = {
      distributedBuilds = true;
      buildMachines = [
        {
          hostName = "rosy";
          maxJobs = 8;
          systems = [ "aarch64-linux" "x86_64-linux" ];
          # Base64-encoded ed25519 public host key of builder:
          #   base64 -w0 /etc/ssh/ssh_host_ed25519_key.pub
          publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSURCbnFmajFrVmVYMDRKWU8xa2lHQjRFaVlqZnNDcE0xUSt6QmpGY0VvZGkgcm9vdEByb3N5Cg==";
          sshUser = config.dotfield.guardian.username;
          # "8 M1 cores, times one (versus times 2 for a desktop)" seems like a reasonable relative speed factor.
          speedFactor = 8 * 1;
          supportedFeatures = [ "nixos-test" "benchmark" "kvm" ];
        }
        # {
        #   hostName = "workstation";
        #   maxJobs = 8;
        #   systems = [ "x86_64-linux" "aarch64-linux" ];
        #   # Base64-encoded ed25519 public host key
        #   publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlMK215amtLR0NZSVlrSTE2NXRxL2NwMDRtMGlveDhSTEViNE1TMXdqZXQgcm9vdEBjZmVlbGV5LXdvcmtzdGF0aW9uCg==";
        #   sshUser = config.dotfield.guardian.username;
        #   # 12 desktop cores, times two (versus times 1 for a laptop)" seems like a reasonable relative speed factor.
        #   speedFactor = 12 * 2;
        #   supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        # }
        # {
        #   # https://github.com/LnL7/nix-docker
        #   # docker run --restart always --name nix-docker -d -p 3022:22 lnl7/nix:ssh
        #   hostName = "nix-docker-build-slave";
        #   systems = [ "x86_64-linux" ];
        #   # Base64-encoded ed25519 public host key
        #   publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlMK215amtLR0NZSVlrSTE2NXRxL2NwMDRtMGlveDhSTEViNE1TMXdqZXQgcm9vdEBjZmVlbGV5LXdvcmtzdGF0aW9uCg==";
        #   # "8 M1 cores, times one (versus times 2 for a desktop)" seems like a reasonable relative speed factor.
        #   speedFactor = 8 * 1;
        #   supportedFeatures = [ "nixos-test" "benchmark" ];
        # }
      ];
    };
  }
]
