{ profiles }:
with profiles; let
  ###: --- roles ---------------------------------------------------------------

  # SSH and shells
  shell = [
    shells.zsh
    shells.fish
    shells.bash
    ssh
  ];

  # beep boop
  developer = [
    direnv
    vim
    git
    shells.zsh
    shells.bash
    shells.fish
    ssh
    ops # nixops
    development.tools
    development.vscode
    python
    virtualisation.common
    ranger
    tealdeer
    vim
  ];

  emacs-config = [
    direnv
    emacs
    git
    development.tools
  ];

  # Desktop packages
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

  # Slightly more frivolous packages
  personalised = [
    shells.fish
    misc
    newsboat
    obs-studio
    spotify
    zotero
    nnn # file manager
  ];

  # Someone else's computer
  server = [
    ssh
  ];

  # If I told you then I'd have to kill you
  trusted = [
    gpg
    # mail
    secrets.common
    secrets.password-store
    yubikey
  ];

  # Web development :(
  webdev = [
    aws
    # nodejs
  ];

  # ∨ ¬ ∧
  fpgadev = [
    development.xilinx
  ];

  # Tools for network diagnostics, information gathering, reverse engineering, etc.
  security = [
    security-tools
  ];

  # OS-specific: Linux-only
  linux = [
    nixos.development.tools
    virtualisation.docker
  ];

  # OS-specific: MacOS
  macos = [
    virtualisation.podman
    darwin.search
  ];

  ###: --- meta-roles ----------------------------------------------------------
  roles = {
    inherit
      shell
      developer
      emacs-config
      graphical
      personalised
      server
      trusted
      webdev
      fpgadev
      security
      linux
      macos
      ;

    workstation =
      shell
      ++ developer
      ++ graphical
      ++ trusted
      ++ webdev
      ++ fpgadev
      ++ security;
  };
in
roles
