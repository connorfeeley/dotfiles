{ collective
, profiles
,
}:
let
  graphical =
    (with collective.profiles; [
      fonts.common
      # TODO: remove?
      # fonts.pragmatapro
    ])
    ++ (with profiles; [
      boot.systemd-boot
      desktop
      gnome-desktop
      video
      # zoom-us
    ]);

  server =
    (with (collective.profiles); [
      networking.common
      networking.tailscale
      networking.ssh-host
    ])
    ++ (with profiles; [ ]);

  tangible =
    (with (collective.profiles); [
      networking.common
      networking.tailscale
    ])
    ++ (with profiles; [
      audio
      bluetooth
      printers-scanners
      networking.wifi
    ]);

  virt = with profiles; [
    virtualisation.libvirtd
    virtualisation.docker
    virtualisation.vagrant
    # virtualisation.virtualbox
  ];

  fpgadev = with profiles; [
    fpga.xilinx
  ];

  desktop =
    (with collective.profiles; [
      networking.ssh-host
      secrets
    ])
    ++ (with profiles; [
      boot.systemd-boot
      yubikey
      networking.avahi
    ]);
in
{
  inherit
    graphical
    server
    tangible
    virt
    fpgadev
    desktop
    ;
}
