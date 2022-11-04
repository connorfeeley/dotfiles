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
  # {
  #   # Serve nix store
  #   nix = {
  #     sshServe = {
  #       enable = true;
  #       protocol = "ssh-ng";
  #       sshUser = config.dotfield.guardian.username;
  #       keys = host.keys;
  #     };
  #   };
  # }

  {
    nix = {
      distributedBuilds = true;
      buildMachines = [
        {
          hostName = "workstation";
          sshUser = config.dotfield.guardian.username;
          systems = [ "x86_64-linux" "aarch64-linux" ];
          # Base64-encoded ed25519 public host key
          publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlMK215amtLR0NZSVlrSTE2NXRxL2NwMDRtMGlveDhSTEViNE1TMXdqZXQgcm9vdEBjZmVlbGV5LXdvcmtzdGF0aW9uCg==";
          # 12 desktop cores, times two (versus times 1 for a laptop)" seems like
          # a reasonable relative speed factor.
          speedFactor = 12 * 2;
          supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        }
        {
          # https://github.com/LnL7/nix-docker
          # docker run --restart always --name nix-docker -d -p 3022:22 lnl7/nix:ssh

          # Host nix-docker-build-slave
          #   User root
          #   HostName 127.0.0.1
          #   Port 3022
          #   IdentityFile "/Users/cfeeley/.nix-docker-build-slave/insecure_rsa"
          hostName = "nix-docker-build-slave";
          systems = [ "x86_64-linux" ];
          # Base64-encoded ed25519 public host key
          publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlMK215amtLR0NZSVlrSTE2NXRxL2NwMDRtMGlveDhSTEViNE1TMXdqZXQgcm9vdEBjZmVlbGV5LXdvcmtzdGF0aW9uCg==";
          # "8 M1 cores, times one (versus times 2 for a desktop)" seems like
          # a reasonable relative speed factor.
          speedFactor = 8 * 1;
          supportedFeatures = [ "nixos-test" "benchmark" ];
        }
      ];
    };
  }
]
