{ self, lib, pkgs, ... }:
let
  inherit (self.collective.peers.hosts) cfeeley-laptop;

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

  # Creates a settings block for ${hostName}.
  # For ${trusted} hosts, enable trusted X11 forwarding and forward the GPG agent socket.
  mkHostBlock = { hostName, trusted ? false }: {
    Port = self.flake-lib.peers.getSshPort hostName;

    # Emacs vterm environment variables
    SendEnv = "INSIDE_EMACS EMACS_VTERM_PATH";

    # X11 forwarding
    ForwardX11 = lib.mkIf trusted true;
    ForwardX11Trusted = lib.mkIf trusted true;

    # GPG agent forwarding
    RemoteForward = lib.mkIf trusted [ gpgAgentForwards ];
  };
in
{
  home.packages = with pkgs; [
    autossh
    sshpass # enter SSH password automatically
  ];

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    includes = [ "~/.config/ssh/config.local" ];

    settings = {
      ### Physical
      "workstation" = mkHostBlock {
        hostName = "workstation";
        trusted = true;
      };
      # "workstation-luks" = { User = "root"; RemoteCommand = "cryptsetup-askpass"; };
      "workstation-luks" = {
        User = "root";
        RemoteCommand = "zfs load-key -a && killall zfs";
      };
      "macbook-pro" = mkHostBlock {
        hostName = "MacBook-Pro";
        trusted = true;
      };
      "cfeeley-laptop" = mkHostBlock
        {
          hostName = "cfeeley-laptop";
          trusted = true;
        } // {
        HostName = cfeeley-laptop.ipv4.address;
      };
      "assuring-redshank" = {
        HostName = "assuring-redshank";
        User = "ubuntu";
      };

      ### VMs (local)
      "rosy" = mkHostBlock {
        hostName = "rosy";
        trusted = true;
      }; # NOTE: manually add entry to root's SSH config (/var/root/.ssh/config) to use as builder

      ### VMs (remote)
      "h8tsner" = mkHostBlock { hostName = "h8tsner"; };

      ### Other
      "github.com" = {
        User = "git";
        ControlMaster = "no";
      };
      "10.*.*.*" = {
        KexAlgorithms = "+diffie-hellman-group1-sha1";
        HostKeyAlgorithms = "+ssh-rsa";
      };

      ### Defaults (replaces old enableDefaultConfig + matchBlocks."*")
      "*" = {
        # From old top-level programs.ssh options
        ForwardAgent = false; # must be disabled to use gpg-agent forwarding
        ServerAliveInterval = 5;
        ServerAliveCountMax = 2;
        Compression = false; # Slow
        ControlPersist = "30s";
        ControlMaster = "auto";

        # From old matchBlocks."*"
        AddressFamily = "inet";
        ForwardX11 = false;
        ForwardX11Trusted = false;

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

        # Use 'throughput' QoS setting for non-interactive sessions
        # default: "af21 cs1" (af21: low-latency data - interactive sessions; cs1: lower effort - non-interactive sessions)
        IPQoS = "af21 throughput";

        # Ciphers = "chacha20-poly1305@openssh.com,aes256-gcm@openssh.com";
        # HostKeyAlgorithms = "ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa";
        # KexAlgorithms = "curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256";
        # MACs = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com";
      };
    };
  };
}
## References:
# https://github.com/drduh/config/blob/master/ssh_config
# https://linux.die.net/man/5/ssh_config
