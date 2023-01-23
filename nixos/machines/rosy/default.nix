{ config
, lib
, pkgs
, profiles
, primaryUser
, collective
, ...
}:
let
  inherit (config.networking) hostName;
  inherit (config.dotfield) guardian;

  # Bootstrap sets up:
  # - enabling systemd-boot
  # - a 'nixos' NixOS and HM user
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Enable automatic login for the user.
  services.xserver.displayManager.autoLogin.enable = false;
  services.xserver.displayManager.autoLogin.user = "cfeeley";

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #  wget
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  # Emulate x86_64-linux with QEMU
  # boot.binfmt.emulatedSystems = [ "x86_64-linux" ];

  # nix.settings.extra-platforms = [ "x86_64-linux" ];

  ### === users ================================================================

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cfeeley";

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$yK0HXWogvyQ5c7qD$pGUcMhDn2W5stXFHPqxmKNdZjQkEHzQgqloWK5fZyOjpQXgJyZ3rKKsaW/.OhWE216AtjN/6PIvmgftQYwtiz.";
  # Authorized keys and PermitRootLogin set in ssh-host
  users.users.cfeeley = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$6$yK0HXWogvyQ5c7qD$pGUcMhDn2W5stXFHPqxmKNdZjQkEHzQgqloWK5fZyOjpQXgJyZ3rKKsaW/.OhWE216AtjN/6PIvmgftQYwtiz.";
    hashedPassword = "$6$yK0HXWogvyQ5c7qD$pGUcMhDn2W5stXFHPqxmKNdZjQkEHzQgqloWK5fZyOjpQXgJyZ3rKKsaW/.OhWE216AtjN/6PIvmgftQYwtiz.";
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups =
      [
        "wheel"
        "video"
        "audio"
        "networkmanager"
        "dialout"
        "cfeeley"
        "secrets"
        "wireshark"
      ]
      ++ (lib.optional config.services.mysql.enable "mysql")
      ++ (lib.optional config.virtualisation.docker.enable "docker")
      ++ (lib.optional config.virtualisation.podman.enable "podman")
    ;

    # Set user's shell
    shell = pkgs.zsh;
  };

  home-manager.users = {
    "${guardian.username}" = hmArgs: {
      imports = with hmArgs.roles; (lib.flatten [
      ] ++ lib.optionals (!config.nixos-vm.enable) (lib.flatten [
        workstation
        developer
        linux
        emacs-config
      ])) ++ (with hmArgs.profiles; [
        sync
        work

        desktop.xmonad

        # Systemd scripts
        nixos.work
      ]);

      programs.termite.enable = false;

      xdg.userDirs.enable = lib.mkForce false;
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBxw8UnnH5Cizu7p9r4PFGDe/azUrdC0qA3K9GtWtvf/+l4dy044X3mI+hHVigTbxDH5viYcTiH6Lk+SHl2uZuX6fkzTBaFoonEJrKeCRS25TTMmas9g7D/maDoENEF1X0acs5Ffk3CAqKlOeynGPnj4M1ovUM8wyg1lsfZXA+LVr9GLLziiZSxVBBjG341hfVP3LFijj8qIAoDnBPrlLBjrrCsHXZa1QxjjyQADC5Ty7wgqLZqhfEEmkSdUEdkEt1lW4wzJzNXM/7F+iBmLTTp2KcUTPP2kyCU8YR+QvOMafB7ufmRoMf2ERjQtCwSJCYfEot3DBOvdgL0lFBTW4T"
  ];

}
