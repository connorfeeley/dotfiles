# FIXME: hardware config
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
# https://github.com/nix-community/nixos-install-scripts/blob/master/hosters/hetzner-dedicated/hetzner-dedicated-wipe-and-install-nixos.sh
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  ### === networking ================================================================

  networking = {
    hostName = "rosy";
    dhcpcd.enable = true;
    networkmanager.enable = true;
  };

  ### === timezone ============================================================

  time.timeZone = "America/Toronto";
  location = {
    provider = "manual";
    latitude = 43.70011;
    longitude = -79.4163;
  };

  ### === users ================================================================

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cfeeley";

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
  # Authorized keys and PermitRootLogin set in ssh-host
  users.users.cfeeley = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
    hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
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
      imports =
        (with hmArgs.roles; shell ++ server ++ trusted) ++
        (with hmArgs.profiles; [ direnv development.tools ]);

      programs.termite.enable = false;
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBxw8UnnH5Cizu7p9r4PFGDe/azUrdC0qA3K9GtWtvf/+l4dy044X3mI+hHVigTbxDH5viYcTiH6Lk+SHl2uZuX6fkzTBaFoonEJrKeCRS25TTMmas9g7D/maDoENEF1X0acs5Ffk3CAqKlOeynGPnj4M1ovUM8wyg1lsfZXA+LVr9GLLziiZSxVBBjG341hfVP3LFijj8qIAoDnBPrlLBjrrCsHXZa1QxjjyQADC5Ty7wgqLZqhfEEmkSdUEdkEt1lW4wzJzNXM/7F+iBmLTTp2KcUTPP2kyCU8YR+QvOMafB7ufmRoMf2ERjQtCwSJCYfEot3DBOvdgL0lFBTW4T"
  ];

  ### === xorg ================================================================

  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  services.xserver = {
    layout = "us";
    xkbVariant = "dvorak";
  };

  # Enable automatic login for the user.
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "cfeeley";

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  ### === misc ================================================================

  console.keyMap = "dvorak";

  services.getty.autologinUser = config.dotfield.guardian.username;

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

  services.openssh = lib.mkDefault {
    enable = true;
    openFirewall = true;
    # Authorized keys and permitRootLogin are set in ssh-host profile
  };

  environment.systemPackages = with pkgs; [
    # SPICE agent is required for clipboard sharing with UTM (both QEMU and Apple backend)
    # as well as dynamic display resolution in QEMU backend.
    spice-vdagent
  ];
}
