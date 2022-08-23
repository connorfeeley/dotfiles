{
  config,
  lib,
  pkgs,
  profiles,
  suites,
  inputs,
  primaryUser,
  collective,
  ...
}: let
  inherit (collective) peers;
  inherit (config.networking) hostName;
  # hostKeys = (lib.our.peers.getHost hostName).keys;
  host = peers.hosts.${hostName};
in
{
  # Serve nix store
  nix = {
    sshServe.enable = true;
    sshServe.keys = host.keys;
  };
}

#   (lib.mkIf (!cfg.serveStore.enable) {
#     # Serve nix store
#     nix = {
#       distributedBuilds = true;
#       buildMachines = [
#         {
#           hostName = "workstation";
#           systems = [ "x86_64-linux" ];
#           sshUser = config.user.name;
#           # Base64-encoded ed25519 public host key
#           publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUlMK215amtLR0NZSVlrSTE2NXRxL2NwMDRtMGlveDhSTEViNE1TMXdqZXQgcm9vdEBjZmVlbGV5LXdvcmtzdGF0aW9uCg==";
#           # "12 desktop cores times two (versus times 1 for a laptop)" seems like
#           # a reasonable relative speed factor.
#           speedFactor = 12 * 2;
#           supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
#         }
#       ];
#     };
#   })
# ];
# }
