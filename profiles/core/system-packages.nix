{ config
, lib
, pkgs
, ...
}:
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
    dua
    duf
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

  environment.shellAliases = {
    ll = "${pkgs.exa} -l";
  };
}
