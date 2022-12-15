moduleArgs @ { config
, lib
, pkgs
, inputs
, ...
}:
let
  inherit (config.lib.dotfield) fsPath;

  sshHostPath =
    if (moduleArgs.impermanence or false)
    then "/persist/etc/ssh"
    else "/etc/ssh";

  # FIXME: is this accurate?
  nixosConfigPath = "${fsPath}/lib/compat/nixos";
in
{
  imports = [
    inputs.nix-ld.nixosModules.nix-ld
    inputs.envfs.nixosModules.envfs
  ];

  nix = {
    settings = {
      auto-optimise-store = true;
      # TODO: is it really reasonable to set these all as defaults?
      system-features = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    };

    nixPath = [ "nixos-config=${nixosConfigPath}" ];
    optimise.automatic = true;
  };

  boot.loader.systemd-boot.consoleMode = "auto";
  boot.cleanTmpDir = lib.mkDefault true;

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
    inetutils
    iputils
    mtr
    pciutils
    sysstat
    usbutils
    util-linux
    smem
    nethogs
    sshfs

    # FHS compat
    nix-alien
    nix-index-update
    nix-index
    nix-autobahn
  ];

  programs.bandwhich.enable = true;

  programs.git.enable = true;
  programs.git.config = {
    safe.directory = [
      "/etc/nixos"
      "/etc/dotfield"
    ];
  };

  programs.mtr.enable = true;

  security.sudo.wheelNeedsPassword = false;

  services.openssh = {
    # For rage encryption, all hosts need a ssh key pair
    enable = lib.mkForce true;

    openFirewall = true;
    passwordAuthentication = false;
    permitRootLogin = lib.mkDefault "prohibit-password";

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
    enableSSHAgentAuth = true;

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

  services.timesyncd.enable = true;
}
