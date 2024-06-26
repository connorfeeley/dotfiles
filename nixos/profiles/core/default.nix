moduleArgs@{ config, lib, pkgs, inputs, ... }:
let
  sshHostPath =
    if (moduleArgs.impermanence or false) then
      "/persist/etc/ssh"
    else
      "/etc/ssh";
in
{
  imports = [ inputs.envfs.nixosModules.envfs ];

  nix = {
    settings = {
      auto-optimise-store = true;
    };

    optimise.automatic = true;
  };

  boot.loader.systemd-boot.consoleMode = "auto";
  boot.tmp.cleanOnBoot = lib.mkDefault true;

  i18n.defaultLocale = lib.mkDefault "en_US.UTF-8";
  i18n.supportedLocales = [ "all" "en_US.UTF-8" ];

  environment.enableAllTerminfo = true;

  environment.shellAliases = {
    # Fix `nixos-option` for flake compatibility
    # FIXME: it's broken
    # nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
  };

  environment.systemPackages = with pkgs; [
    dnsutils
    dosfstools
    efibootmgr
    gptfdisk
    iputils
    mtr
    pciutils
    sysstat
    usbutils
    util-linux
    smem
    nethogs
    sshfs
    lsof

    # FHS compat
    nix-alien
    nix-index-update
    nix-index
    nix-autobahn

    nixos-install-tools # in the *totally unlikely* case I need to recover my system
  ];
  programs.nix-ld.enable = true;

  programs.bandwhich.enable = true;

  programs.git.enable = true;
  programs.git.config = { safe.directory = [ "/etc/nixos" "/etc/dotfiles" ]; };

  programs.mtr.enable = true;

  security.sudo.wheelNeedsPassword = false;

  services.openssh = {
    # For rage encryption, all hosts need a ssh key pair
    enable = lib.mkForce true;

    openFirewall = true;
    settings.PasswordAuthentication = false;
    settings.PermitRootLogin = lib.mkDefault "prohibit-password";

    hostKeys = [
      {
        bits = 4096;
        path = "${sshHostPath}/ssh_host_rsa_key";
        type = "rsa";
      }
      {
        path = "${sshHostPath}/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ];
  };

  security.pam = {
    # Allow passwordless sudo within an SSH session.
    sshAgentAuth.enable = true;

    # Raise number of allowed open file descriptors
    loginLimits = [{
      domain = "*";
      type = "soft";
      item = "nofile";
      value = "8192"; # Default: 1024
    }];
  };

  hardware.enableRedistributableFirmware = lib.mkDefault true;

  environment.enableDebugInfo = true;

  documentation = {
    # NOTE: Cross-platform documentation options are set in 'profiles/core'.
    dev.enable = true;
    man = rec {
      generateCaches = true;
      # Mandoc isn't compatible with MANPAGER=bat: https://github.com/sharkdp/bat/issues/1145
      mandoc.enable = false;
      man-db.enable = !mandoc.enable;
    };
    nixos.enable = true;
  };

  services.timesyncd.enable = lib.mkDefault true;

  location = {
    provider = "manual";
    latitude = 43.70011;
    longitude = -79.4163;
  };

  programs.ccache.enable = true;
}
