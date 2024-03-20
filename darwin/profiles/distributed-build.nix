{ config, lib, collective, ... }:
let inherit (config.networking) hostName;
in {
  nix = {
    # FIXME: keep in sync with below
    settings.builders = [
      "cfeeley@workstation x86_64-linux,aarch64-linux,i686-linux - 12 24 nixos-test,benchmark,big-parallel,kvm - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlMK215amtLR0NZSVlrSTE2NXRxL2NwMDRtMGlveDhSTEViNE1TMXdqZXQgcm9vdEBjZmVlbGV5LXdvcmtzdGF0aW9uCg=="
      "cfeeley@rosy aarch64-linux - 12 24 nixos-test,benchmark,kvm - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUdndzd2Slpoa1E1bU1PekxUVXROeHdwbStNc2VFWUJSY0V4YUUwTnZaZG4gcm9vdEByb3N5Cg=="
    ];

    distributedBuilds = true;
    buildMachines = [
      ### NixOS workstation
      {
        hostName = "workstation";
        maxJobs = 12;
        systems = [ "x86_64-linux" "aarch64-linux" "i686-linux" ];
        # Base64-encoded ed25519 public host key of builder:
        #   base64 -w0 /etc/ssh/ssh_host_ed25519_key.pub
        publicHostKey =
          "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlMK215amtLR0NZSVlrSTE2NXRxL2NwMDRtMGlveDhSTEViNE1TMXdqZXQgcm9vdEBjZmVlbGV5LXdvcmtzdGF0aW9uCg==";
        sshUser = config.dotfield.guardian.username;
        # 12 desktop cores, times two (versus times 1 for a laptop)" seems like a reasonable relative speed factor.
        speedFactor = 12;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      }
      ### NixOS VM (Parallels)
      {
        hostName = "rosy";
        maxJobs = 8;
        systems = [ "aarch64-linux" "x86_64-linux" ];
        # Base64-encoded ed25519 public host key of builder:
        #   base64 -w0 /etc/ssh/ssh_host_ed25519_key.pub
        # publicHostKey = (collective.peers.hosts.rosy).keys.0;
        publicHostKey =
          "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUdndzd2Slpoa1E1bU1PekxUVXROeHdwbStNc2VFWUJSY0V4YUUwTnZaZG4gcm9vdEByb3N5Cg==";

        sshUser = config.dotfield.guardian.username;
        # sshKey = config.age.secrets."workstation-luks/ssh_host_ed25519_key".path;

        # "8 M1 cores, times one (versus times 2 for a desktop)" seems like a reasonable relative speed factor.
        speedFactor = 8 * 1;
        supportedFeatures = [ "nixos-test" "benchmark" "kvm" ];
      }
    ];
  };
}
