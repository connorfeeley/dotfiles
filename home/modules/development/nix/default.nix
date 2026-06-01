# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ inputs', config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption;

  cfg = config.development.nix;
in
{
  options.development.nix = {
    enable = mkEnableOption "Nix development tools";
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      # :lang nix
      pkgs.nixpkgs-fmt
      pkgs.nil # ('nix-nil' from source repo)
      inputs'.nixd.packages.nixd
      pkgs.nixfmt-rfc-style
    ];
  };
}
