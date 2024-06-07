# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, lib, pkgs, ... }:

let
  cfg = config.nix.caches;
in
{
  options.nix.caches = {
    enable = lib.mkEnableOption "Use personal caches as Nix substituters.";
    attic.enable = lib.mkEnableOption "Use personal private Attic cache as Nix substituter.";
    attic.upload.enable = lib.mkEnableOption "Set post-build-hook to automitically upload to Attic cache.";
    cachix.enable = lib.mkEnableOption "Use personal public Cachix cache as Nix substituter." // { default = true; };
  };

  config = lib.mkIf cfg.enable {
    # Client tools
    environment.systemPackages = [ ] ++
      lib.optionals cfg.attic.enable [ pkgs.attic-client ] ++
      lib.optionals cfg.cachix.enable [ pkgs.cachix ]
    ;

    # Use personal caches
    nix.settings = {
      extra-substituters = [
        (lib.optionalString cfg.attic.enable "https://workstation.elephant-vibes.ts.net/cfeeley")
        (lib.optionalString cfg.cachix.enable "https://cfeeley.cachix.org")
      ];
      extra-trusted-public-keys = [
        (lib.optionalString cfg.attic.enable "cfeeley:42CV0l4NnVzAhk7xGYuLUgvKY6fjwfQplH77eH+PBmI=")
        (lib.optionalString cfg.cachix.enable "cfeeley.cachix.org-1:b+RrHsy/4WWys2o6T4YyF66OhdiZUF/R/N46JcS0HJU=")
      ];

      # Automatically upload to attic cache
      post-build-hook = lib.mkIf cfg.attic.upload.enable (pkgs.writeShellScript "hook" ''
        set -eu
        set -f # disable globbing
        export IFS=' '

        exec ${pkgs.attic}/bin/attic push workstation:cfeeley $OUT_PATHS $DRV_PATH
      '');
    };
  };
}
