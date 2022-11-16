moduleArgs @ { config
, lib
, pkgs
, peers
, ...
}:
let
  inherit (peers.hosts) workstation MacBook-Pro cfeeley-laptop h8tsner rosy;
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
      "workstation" = {
        forwardX11Trusted = true;
        extraOptions.SendEnv = "INSIDE_EMACS EMACS_VTERM_PATH";
        # GPG agent forwarding
        remoteForwards = [{
          bind.address = "/run/user/1000/gnupg/S.gpg-agent";
          host.address = "/Users/cfeeley/.gnupg/S.gpg-agent.extra";
        }];
      };
      "workstation-luks" = { user = "root"; };
      "macbook-pro" = {
        extraOptions.SendEnv = "INSIDE_EMACS EMACS_VTERM_PATH";
        # GPG agent forwarding
        remoteForwards = [{
          bind.address = "/Users/cfeeley/.gnupg/S.gpg-agent";
          host.address = "/run/user/1000/gnupg/S.gpg-agent.extra";
        }];
      };
      # NOTE: manually add entry to root's SSH config (/var/root/.ssh/config) to use as builder
      "rosy" = {
        forwardX11Trusted = true;
        extraOptions.SendEnv = "INSIDE_EMACS EMACS_VTERM_PATH";
        # GPG agent forwarding
        remoteForwards = [{
          bind.address = "/run/user/1000/gnupg/S.gpg-agent";
          host.address = "/Users/cfeeley/.gnupg/S.gpg-agent.extra";
        }];
      };
      "h8tsner" = {
        hostname = h8tsner.ipv4.address;
        # GPG agent forwarding
        remoteForwards = [{
          bind.address = "/run/user/1000/gnupg/S.gpg-agent";
          host.address = "/Users/cfeeley/.gnupg/S.gpg-agent.extra";
        }];
      };
      "cfeeley-laptop" = {
        hostname = cfeeley-laptop.ipv4.address;
        user = "cfeeley";
        forwardX11 = true;
        extraOptions.SendEnv = "INSIDE_EMACS EMACS_VTERM_PATH";
        # GPG agent forwarding
        remoteForwards = [{
          bind.address = "/run/user/1000/gnupg/S.gpg-agent";
          host.address = "/Users/cfeeley/.gnupg/S.gpg-agent.extra";
        }];
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
    };
  };
}
## References:
# https://github.com/drduh/config/blob/master/ssh_config
# https://linux.die.net/man/5/ssh_config

