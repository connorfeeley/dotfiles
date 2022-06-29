let
  inherit (builtins) readFile;
  inherit (peers) hosts;
  peers = import ../ops/metadata/peers.nix;
  yubiGpg = readFile ./ssh-yubikey.pub;
in
with hosts;
[yubiGpg]
++ hierophant.keys
++ hierophant.users.hierophant.keys
++ boschic.users.seadoom.keys
++ hodgepodge.users.seadoom.keys
++ tapestone.users.seadoom.keys
++ aerattum.users.blink.keys
++ aerattum.users.workingcopy.keys
