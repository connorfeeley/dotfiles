{profiles}:
with profiles; let
  developer = [
    direnv
    emacs
    git
    shells.zsh
    shells.fish
    ssh
    virtualisation.docker
  ];

  graphical = [
    firefox
    chromium
    foot
    keyboard
    kitty
    misc
    mpv
    themes
  ];

  graphical-desktop = [
    desktop.common
    desktop.gnome
    desktop.xmonad
  ];

  personalised = [
    espanso
    misc
    newsboat
    obs-studio
    spotify
    sync
    zotero
  ];

  remote = [
    shells.fish
    ssh
  ];

  trusted = [
    gpg
    # mail
    # FIXME(darwin): broken
    # promnesia
    secrets.one-password
    secrets.password-store
    yubikey
    work
  ];

  webdev = [
    aws
    nodejs
    development.php
    development.wordpress
  ];

  fpgadev = [
    development.xilinx
  ];

  security = [
    security-tools
  ];

  roles = {
    inherit
      developer
      graphical
      personalised
      remote
      trusted
      webdev
      fpgadev
      security
      ;

    workstation =
      developer
      ++ graphical
      ++ personalised
      ++ trusted
      ++ webdev
      ++ fpgadev
      ++ security;

    workstation-small =
      developer
      ++ personalised
      ++ trusted
      ++ webdev
      ++ fpgadev;
  };
in
  roles
