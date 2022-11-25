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
  inherit (config.lib.dotfield.secrets) secretsDir secretsGroup mkAgeSecret;
  host = peers.hosts.${hostName};

  workstation-priv = { file = "${secretsDir}/hosts/workstation/cache-priv-key.pem.age"; group = secretsGroup; };
  workstation-pub = { file = "${secretsDir}/hosts/workstation/cache-pub-key.pem.age"; group = secretsGroup; };

  macbook-pro-priv = { file = "${secretsDir}/hosts/macbook-pro/cache-priv-key.pem.age"; group = secretsGroup; };
  macbook-pro-pub = { file = "${secretsDir}/hosts/macbook-pro/cache-pub-key.pem.age"; group = secretsGroup; };
in
lib.mkMerge [
  {
    nix = {
      distributedBuilds = true;
      buildMachines = [
        ### NixOS workstation
        {
          hostName = "workstation";
          maxJobs = 8;
          systems = [ "x86_64-linux" "aarch64-linux" ];
          # Base64-encoded ed25519 public host key of builder:
          #   base64 -w0 /etc/ssh/ssh_host_ed25519_key.pub
          publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlMK215amtLR0NZSVlrSTE2NXRxL2NwMDRtMGlveDhSTEViNE1TMXdqZXQgcm9vdEBjZmVlbGV5LXdvcmtzdGF0aW9uCg==";
          sshUser = config.dotfield.guardian.username;
          # 12 desktop cores, times two (versus times 1 for a laptop)" seems like a reasonable relative speed factor.
          speedFactor = 12 * 2;
          supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        }
        ### NixOS VM (UTM)
        {
          hostName = "rosy";
          maxJobs = 8;
          systems = [ "aarch64-linux" "x86_64-linux" ];
          # Base64-encoded ed25519 public host key of builder:
          #   base64 -w0 /etc/ssh/ssh_host_ed25519_key.pub
          publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSURCbnFmajFrVmVYMDRKWU8xa2lHQjRFaVlqZnNDcE0xUSt6QmpGY0VvZGkgcm9vdEByb3N5Cg==";

          sshUser = config.dotfield.guardian.username;
          # sshKey = config.age.secrets."workstation-luks/ssh_host_ed25519_key".path;

          # "8 M1 cores, times one (versus times 2 for a desktop)" seems like a reasonable relative speed factor.
          speedFactor = 8 * 1;
          supportedFeatures = [ "nixos-test" "benchmark" "kvm" ];
        }
      ];
    };
  }
]
