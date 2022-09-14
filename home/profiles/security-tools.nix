{ config
, lib
, pkgs
, ...
}:

{
  home.packages = with pkgs; [
    ## === Reverse Engineering ===
    (binwalk.override { visualizationSupport = true; })
    radare2
    radare2-cutter

    ## === Crypto-Stego ===
    ccrypt

    ## === Forensics ===
    chkrootkit
    ddrescue
    exiv2
    pdf-parser

    ## === Networking ===
    arping
    nmap
    ncrack
    aircrack-ng
    wpscan
    tcpreplay
    tcpdump
    wireshark
    proxychains
    udptunnel
    macchanger
    mitmproxy
    spike # Network protocol fuzzer
    sslsplit

    ## === Information Gathering ===
    theharvester

    ## === Passwords ===
    thc-hydra
    john
  ] ++ (lib.optionals pkgs.stdenv.isLinux [
    ## === Passwords ===
    hashcat
    hashcat-utils
  ]);
}
