{ config, lib, collective, ... }:
let inherit (config.networking) hostName;
in {
  nix = {
    # Pull already-built store paths from proxmox-builder over SSH. It signs its
    # store with the shared workstation key (nixos/modules/substituter.nix), so
    # the matching public key must be trusted here.
    settings.extra-substituters = [
      "ssh-ng://cfeeley@proxmox-builder?ssh-key=/etc/nix/proxmox_builder_ed25519"
    ];
    settings.extra-trusted-public-keys = [
      "workstation.elephant-vibes.ts.net:10NdZltvaHQ+R4zJ+Vze7V2sHyy8fo6mq0me2mTrqho="
    ];

    distributedBuilds = true;
    # Single source of truth for remote builders. nix-darwin writes these to
    # /etc/nix/machines, which Nix reads via its default `builders =
    # @/etc/nix/machines`. (Do not also set `nix.settings.builders` to a literal
    # string: that overrides the default and shadows this file, including any
    # builders appended by other modules such as nix.linux-builder.)
    buildMachines = [
      ### NixOS workstation
      {
        hostName = "workstation";
        protocol = "ssh-ng";
        maxJobs = 12;
        systems = [ "x86_64-linux" "aarch64-linux" "i686-linux" ];
        # Base64-encoded ed25519 public host key of builder:
        #   base64 -w0 /etc/ssh/ssh_host_ed25519_key.pub
        publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlMK215amtLR0NZSVlrSTE2NXRxL2NwMDRtMGlveDhSTEViNE1TMXdqZXQgcm9vdEBjZmVlbGV5LXdvcmtzdGF0aW9uCg==";
        sshUser = config.dotfiles.guardian.username;
        sshKey = "/etc/nix/workstation_ed25519";
        # 12 desktop cores, times two (versus times one for a laptop) is a reasonable relative speed factor.
        speedFactor = 24;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      }
      ### NixOS rosy (aarch64-linux). No sshKey: relies on the daemon's SSH config.
      {
        hostName = "rosy";
        protocol = "ssh-ng";
        maxJobs = 12;
        systems = [ "aarch64-linux" ];
        publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUdndzd2Slpoa1E1bU1PekxUVXROeHdwbStNc2VFWUJSY0V4YUUwTnZaZG4gcm9vdEByb3N5Cg==";
        sshUser = config.dotfiles.guardian.username;
        speedFactor = 24;
        supportedFeatures = [ "nixos-test" "benchmark" "kvm" ];
      }
      ### NixOS proxmox-builder (local Proxmox VM)
      {
        hostName = "proxmox-builder";
        protocol = "ssh-ng";
        maxJobs = 8;
        systems = [ "x86_64-linux" "aarch64-linux" "i686-linux" ];
        # Base64-encoded ed25519 public host key of builder:
        #   base64 -w0 /etc/ssh/ssh_host_ed25519_key.pub
        publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUd1dDhFM1hLVzNZcVpHMm5VVGxXU3FzdWNJQ2NNYzFBeml1Ky9ocWQ0M0Mgcm9vdEBwcm94bW94LWJ1aWxkZXIK";
        sshUser = config.dotfiles.guardian.username;
        sshKey = "/etc/nix/proxmox_builder_ed25519";
        speedFactor = 8;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      }
    ];
  };
}
