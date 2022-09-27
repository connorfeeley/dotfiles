{ config
, lib
, pkgs
, ...
}:

{
  home.packages = with pkgs; [
    ## === Reverse Engineering ===
    (binwalk.override { visualizationSupport = pkgs.stdenv.isLinux; })

    ## === Crypto-Stego ===
    ccrypt

    ## === Forensics ===
    chkrootkit
    ddrescue
    exiv2
    pdf-parser
    bingrep # Greps through binaries from various OSs and architectures, and colors them.

    ## === Networking ===
    arping
    nmap
    ncrack
    wpscan
    socat
    tcpreplay
    tcpdump
    wireshark
    sslsplit
    rustscan
  ] ++ (lib.optionals pkgs.stdenv.isLinux [
    ## === Reverse Engineering ===
    (binwalk.override { visualizationSupport = true; })
    radare2
    radare2-cutter

    ## === Networking ===
    aircrack-ng
    mitmproxy
    proxychains
    udptunnel
    macchanger
    spike # Network protocol fuzzer

    ## === Information Gathering ===
    theharvester
    poppler_utils # pdf rendering tools

    ## === Passwords ===
    thc-hydra
    john
    hashcat
    hashcat-utils
  ]);
}
