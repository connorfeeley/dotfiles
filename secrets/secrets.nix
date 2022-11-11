{ config
, lib
, pkgs
, inputs
, ...
}:
let
  inherit (peers) hosts;
  peers = import ../ops/metadata/peers.nix;
  hostKeys = builtins.mapAttrs (n: v: v.keys) hosts;
  trustedUsers = import ./authorized-keys.nix;

  servers = with hostKeys;
    hierophant
    ++ tsone;

  workstations = with hostKeys;
    workstation
    ++ hodgepodge;

  allMachines = servers ++ workstations;

  inherit (pkgs.stdenv.hostPlatform) isLinux system;
  # TODO: impermanence
  hasImpermanence = false;

  cfg = config.age;
  secretsDir = ./age;
  sshPath = "/etc/ssh";

  # nix-darwin does not support the `users.<name>.extraGroups` option, but
  # that's not a problem since we're only using darwin systems as a single
  # admin user. although the username may vary across systems, each "primary
  # user" will still be in the `admin` group.
  secretsGroup =
    if isLinux
    then "secrets"
    else "admin";

  mkAgeSecret = name: {
    "${name}" = {
      file = "${secretsDir}/${name}.age";
      group = secretsGroup;
      # path = "${cfg.secretsDir}/${name}";
    };
  };
in
{
  age.secrets = lib.mkMerge [
    (mkAgeSecret "minecraft-rcon-password.txt")
    (mkAgeSecret "tailscale-luks-setup.state")
    (mkAgeSecret "ssh_host_ed25519_key")
    (mkAgeSecret "ssh_host_ed25519_key.pub")
    (mkAgeSecret "ssh_host_rsa_key")
    (mkAgeSecret "ssh_host_rsa_key.pub")
  ];
}
