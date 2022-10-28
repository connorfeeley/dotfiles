moduleArgs @ { config
, lib
, pkgs
, peers
, ...
}:
let
  inherit (peers.hosts) workstation MacBook-Pro cfeeley-laptop;
  identityFileName = "id_ed25519.pub";
  identityFile = "~/.ssh/${identityFileName}";
in
{
  programs.ssh = {
    enable = true;
    forwardAgent = false; # SSH agent forwarding must be disabled to use gpg-agent forwarding
    serverAliveInterval = 5;
    serverAliveCountMax = 2;
    compression = false; # Slow
    controlPersist = "10m";
    controlMaster = "auto";

    includes = [ "~/.config/ssh/config.local" ];

    matchBlocks = {
      "workstation" = {
        forwardX11Trusted = true;
        remoteForwards = [{
          bind.address = "/run/user/1000/gnupg/S.gpg-agent";
          host.address = "/Users/cfeeley/.gnupg/S.gpg-agent.extra";
        }];
        extraOptions.SendEnv = "DISPLAY INSIDE_EMACS EMACS_VTERM_PATH";
      };
      "macbook-pro" = {
        forwardX11Trusted = true;
        remoteForwards = [{
          bind.address = "/Users/cfeeley/.gnupg/S.gpg-agent";
          host.address = "/run/user/1000/gnupg/S.gpg-agent.extra";
        }];
        extraOptions.SendEnv = "DISPLAY INSIDE_EMACS EMACS_VTERM_PATH";
      };
      "cfeeley-laptop" = {
        hostname = cfeeley-laptop.ipv4.address;
        user = "cfeeley";
      };

      "github.com" = {
        # inherit identityFile;
        # identitiesOnly = true;
        user = "git";
        extraOptions = {
          # "MACs" = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com";
        };
        extraOptions = {
          ControlMaster = "no";
        };
      };

      "10.*.*.*" = {
        extraOptions = {
          KexAlgorithms = "+diffie-hellman-group1-sha1";
          HostKeyAlgorithms = "+ssh-rsa";
        };
      };

      "*" = {
        addressFamily = "inet";
        forwardX11 = false;
        forwardX11Trusted = false;

        extraOptions = {
          AddKeysToAgent = "yes";
          ChallengeResponseAuthentication = "no";
          PasswordAuthentication = "yes";
          StrictHostKeyChecking = "ask";
          VerifyHostKeyDNS = "yes";
          VisualHostKey = "yes";

          PubkeyAcceptedKeyTypes = "+ssh-rsa";
          KexAlgorithms = "+diffie-hellman-group1-sha1";
          HostKeyAlgorithms = "+ssh-rsa";
          # Ciphers = "chacha20-poly1305@openssh.com,aes256-gcm@openssh.com";
          # HostKeyAlgorithms = "ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa";
          # KexAlgorithms = "curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256";
          # MACs = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com";

          # Put John Nagle on a timeout. Har har, get it?
          # (disable Nagle algorithm)
          # FIXME: This may not even be an actual configuration option.
          # NoDelay = "yes" ;
        };
      };
    };
  };
}
## References:
# https://github.com/drduh/config/blob/master/ssh_config
# https://linux.die.net/man/5/ssh_config

