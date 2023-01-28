{ config
, lib
, ...
}:
# let
#   inherit (config.networking) hostName;
#   hostNet =
#     (self.lib.peers.getHost hostName).network
#       or (config.nixos-vm.peerConfig).network or null;
# in
lib.mkMerge [
  {
    networking = {
      # Use Cloudflare DNS
      # https://developers.cloudflare.com/1.1.1.1/
      nameservers = lib.mkIf (!config.wsl.wslConf.network.generateResolvConf) [
        "1.1.1.1"
        "1.0.0.1"
        "2606:4700:4700::1111"
        "2606:4700:4700::1001"
      ];
    };
  }
  # FIXME: adds domain to homeConfigurations (<user>@<host><domain>), causing endless errors.
  # I don't want to deal with it.
  # (lib.mkIf (config.networking ? domain) {
  #   networking.domain = (self.lib.peers.getNet hostNet).domain or null;
  # })
]
