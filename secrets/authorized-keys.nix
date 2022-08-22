let
  inherit (builtins) readFile;
  inherit (peers) hosts;
  peers = import ../ops/metadata/peers.nix;
  yubiGpg = readFile ./ssh-yubikey.pub;
in
  with hosts;
    [yubiGpg]
    ++ workstation.keys
