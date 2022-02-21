{ pkgs, lib, config, ... }:

let
  inherit (config.my.keys) sshHostKeyPaths;
in

{
  # Ensure correct permissions
  # TODO: is this an issue with home-manager or nix-darwin, or a result of my config?
#  system.activationScripts.postUserActivation.text =
#    lib.concatMapStrings (f: "chmod 600 ${f}\n")
#      (sshHostKeyPaths);

  my.hm.programs.ssh = {
    enable = true;
    hashKnownHosts = true;
    forwardAgent = false;
    serverAliveInterval = 300;
    serverAliveCountMax = 2;

    includes = [ "${config.my.user.home}/.config/ssh/config.local" ];

    matchBlocks = {

      "github.com" = {
        user = "git";
        identitiesOnly = true;
        extraOptions = {
          "MACs" = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com";
        };
        extraOptions = {
          ControlMaster = "no";
        };
      };

      "*" = {
        addressFamily = "inet";
        forwardX11 = false;
        forwardX11Trusted = false;
        identityFile = "${config.my.user.home}/.ssh/id_ed25519";
        serverAliveInterval = 300;
        serverAliveCountMax = 2;

        extraOptions = {
          AddKeysToAgent = "yes";
          ChallengeResponseAuthentication = "no";
          PasswordAuthentication = "no";
          StrictHostKeyChecking = "ask";
          VerifyHostKeyDNS = "yes";
          VisualHostKey = "yes";

          Ciphers = "chacha20-poly1305@openssh.com,aes256-gcm@openssh.com";
          HostKeyAlgorithms = "ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa";
          KexAlgorithms = "curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256";
          MACs = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com";
        };
      };

    };
  };
}


## References:
# https://github.com/drduh/config/blob/master/ssh_config
# https://linux.die.net/man/5/ssh_config
