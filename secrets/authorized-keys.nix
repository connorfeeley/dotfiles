let
  inherit (builtins) readFile;
  inherit (peers) hosts;
  peers = import ../ops/metadata/peers.nix;
  yubiGpg = readFile ./ssh-yubikey.pub;
  macGpg = readFile ./ssh-macgpg.pub;
in
with hosts;
[ yubiGpg macGpg ]
++ workstation.keys
