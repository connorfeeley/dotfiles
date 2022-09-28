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
    desktop.common
    # desktop.gnome
    desktop.xmonad
    desktop.gui
    firefox
    chromium
    foot
    keyboard
    kitty
    misc
    mpv
    themes
  ];

  personalised = [
    apple-music
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
    mail
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
  };
in
  roles
