{profiles}:
with profiles; let
  developer = [
    direnv
    emacs
    git
    shells.zsh
    shells.bash
    shells.fish
    ssh
    development.tools
    virtualisation.docker
  ];

  graphical = [
    desktop.common
    # desktop.gnome
    desktop.gui
    desktop.word-processing
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
    misc
    newsboat
    obs-studio
    spotify
    zotero
  ];

  remote = [
    shells.fish
    ssh
  ];

  trusted = [
    gpg
    mail
    secrets.password-store
    yubikey
    work
    sync
  ];

  webdev = [
    aws
    nodejs
  ];

  fpgadev = [
    development.xilinx
  ];

  security = [
    security-tools
  ];

  linux = [
    desktop.xmonad
    nixos.development.tools
  ];

  macos = [
    virtualisation.podman

    darwin.search
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
      linux
      macos
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
