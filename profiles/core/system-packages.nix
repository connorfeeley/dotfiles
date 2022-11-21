{ config
, lib
, pkgs
, ...
}:
let
  inherit (config.lib) dotfield;
in
{
  environment.systemPackages = with pkgs; [
    hlissner-hey

    ## === Essentials ===
    bashInteractive
    bat
    binutils
    cacert
    coreutils-full
    exa
    fd
    findutils
    gawk
    git
    gnumake
    gnupg
    gnused
    gnutar
    grc
    jq
    less
    openssh
    openssl
    ripgrep
    rsync
    screen
    vim

    ## === Network ===
    curl
    nmap
    wget
    whois
  ];
}
