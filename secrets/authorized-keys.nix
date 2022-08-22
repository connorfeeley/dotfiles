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
    ++ boschic.users.cfeeley.keys
    ++ hodgepodge.users.cfeeley.keys
    ++ tsone.users.cdom.keys
    ++ aerattum.users.blink.keys
    ++ aerattum.users.workingcopy.keys
    ++ brakhage.users.blink.keys
