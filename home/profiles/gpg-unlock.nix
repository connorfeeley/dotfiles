# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

# This profile adds a script to trigger the GPG password prompt.

{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux isDarwin isAarch64;

  # Arbitrary file encrypted with my GPG key.
  encryptedFile = pkgs.writeText "encrypted-file.asc" ''
    -----BEGIN PGP MESSAGE-----

    hQIMA5gW7CwiMH1cARAAin3wi8OwbpPgkHsbe8FcjeT7v5ZL96NRP6LQCxEbKPZ/
    DfUp9WHdAo3F6ed3L5Qc52fmjuGWnmAZnZYKC23eN6PQ/9lSbv1lbTQP6rPd+bvL
    DpjCPX6urWmDSR4LBRRYTbhUGckR7OpCYLa9ogC5PfxImJ2iSWcDIkl6O+3MQRGy
    6lznpTlsdRuI3NawoaezjzZfTBflkvFEFH+YqBSAmgKpXT3/NrsD33JAUXuDjwYs
    G84dQKb9AtpozMxTQmCs0uSE6w/qxdFpLzkrqWLivJSqMyL5yvdQIWpEyegg5Ppv
    54UnmPDY6GB8qMusne65dsBRvAnp+WOuY7I9GJBXVuDjvRtJR0tcrskOB52f7EFf
    sL6yogJyb7nN0N7/zXr9/Ey2Otoze+94HUu22+RtlWlwIo/EA/O0pHU5rfpSwG/s
    I+X+9ADgDoeXQASapLHdQmdHtXKpe8tGyoeGj7zSeO0KKCmn5/PKPynFlDLTA5Pt
    0JaSonQ8VUEYIB30Ip0YrsShN1eEu/v+BK4wC4oUZjL5tthVqXYBracXdLMZJF1H
    P8fRKXGlKPy3p49WQ/UwH0SkK/ShM3YBTI7hZOgCZ+Dk5hitu9ByaCMUNRyb7pi3
    XU7XP8Z+RRB3p5ZtPW9Q1djO2zswrSMN9oh7BZWmNV7diGzz1nXFDIsR3ST+GmDS
    RAEXlVZbMbfQ+Utm2JFjEhjg3+08vxoPlZqHtxJSxbiPejIKTg7RkSXjDwAzfLbM
    w9roEtqJTPGGWifrRKAfy9xakaOm
    =DTo3
    -----END PGP MESSAGE-----
  '';

  # Script to trigger the GPG password prompt.
  unlock-gpg = pkgs.writeShellScriptBin "unlock-gpg" ''
    echo "Enter password for GPG key:"
    stty -echo
    gpg --pinentry-mode loopback --decrypt ${encryptedFile}
    stty echo
  '';
in
{
  home.packages = [ unlock-gpg ];
}
