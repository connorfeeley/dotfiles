# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ inputs', config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption;

  cfg = config.development.rust;
in
{
  options.development.rust = {
    enable = mkEnableOption "Rust development tools";
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.cargo
      pkgs.rust-analyzer
      pkgs.rustfmt
    ] ++ (lib.optional pkgs.stdenv.isLinux [
      pkgs.clippy
    ]);
  };
}
