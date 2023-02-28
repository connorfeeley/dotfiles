{ lib, pkgs, peers, ... }:
let
  inherit (peers.hosts) cfeeley-laptop;

  inherit (pkgs.stdenv) isDarwin;

  gpgAgentForwards = {
    # if isDarwin: forward GPG-agent extra socket (~/.gnupg/S.gpg-agent.extra) to remote
    # if isLinux: forward GPG-agent extra socket (/run/user/1000/gnupg/S.gpg-agent.extra) to remote
    host.address =
      if isDarwin then
        "/Users/cfeeley/.gnupg/S.gpg-agent.extra"
      else
        "/run/user/1000/gnupg/S.gpg-agent.extra";
    bind.address =
      if isDarwin then
        "/run/user/1000/gnupg/S.gpg-agent.extra"
      else
        "/Users/cfeeley/.gnupg/S.gpg-agent.extra";
  };

  # Creates a matchBlock for ${hostName}.
  # For ${trusted} hosts, enable trusted X11 forwarding and forward the GPG agent socket.
  mkMatchBlock = { hostName, trusted ? false }: {
    port = lib.our.peers.getSshPort hostName;

    # Emacs vterm environment variables
    extraOptions.SendEnv = "INSIDE_EMACS EMACS_VTERM_PATH";

    # X11 forwarding
    forwardX11 = lib.mkIf (trusted) true;
    forwardX11Trusted = lib.mkIf (trusted) true;

    # GPG agent forwarding
    remoteForwards = lib.mkIf (trusted) [ gpgAgentForwards ];
  };
in
{
  home.packages = with pkgs; [ autossh ];

  programs.ssh = {
    enable = true;
    forwardAgent =
      false; # SSH agent forwarding must be disabled to use gpg-agent forwarding
    serverAliveInterval = 5;
    serverAliveCountMax = 2;
    compression = false; # Slow
    controlPersist = "5m";
    controlMaster = "auto";

    includes = [ "~/.config/ssh/config.local" ];

    matchBlocks = {
      ### Physical
      "workstation" = mkMatchBlock {
        hostName = "workstation";
        trusted = true;
      };
      # "workstation-luks" = { user = "root"; extraOptions.RemoteCommand = "cryptsetup-askpass"; };
      "workstation-luks" = {
        user = "root";
        extraOptions.RemoteCommand = "zfs load-key -a && killall zfs";
      };
      "macbook-pro" = mkMatchBlock {
        hostName = "MacBook-Pro";
        trusted = true;
      };
      "cfeeley-laptop" = mkMatchBlock
        {
          hostName = "cfeeley-laptop";
          trusted = true;
        } // {
        hostname = cfeeley-laptop.ipv4.address;
      };
      "assuring-redshank" = {
        hostname = "assuring-redshank";
        user = "ubuntu";
      };

      ### VMs (local)
      "rosy" = mkMatchBlock {
        hostName = "rosy";
        trusted = true;
      }; # NOTE: manually add entry to root's SSH config (/var/root/.ssh/config) to use as builder

      ### VMs (remote)
      "h8tsner" = mkMatchBlock { hostName = "h8tsner"; };

      ### Other
      "github.com" = {
        user = "git";
        extraOptions = { ControlMaster = "no"; };
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
          Port = "22";
          # Ciphers = "chacha20-poly1305@openssh.com,aes256-gcm@openssh.com";
          # HostKeyAlgorithms = "ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa";
          # KexAlgorithms = "curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256";
          # MACs = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com";
        };
      };
    };
  };
}
## References:
# https://github.com/drduh/config/blob/master/ssh_config
# https://linux.die.net/man/5/ssh_config
