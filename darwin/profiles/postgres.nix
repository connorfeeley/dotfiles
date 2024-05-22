# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, pkgs, ... }:

{
  services.postgresql = rec {
    enable = true;
    package = pkgs.postgresql_16;
    extraPlugins = with package.pkgs; [
      pg_cron
      pg_partman
      # postgis # broken
      # timescaledb # broken
      # timescaledb_toolkit # broken
    ];
    initdbArgs = [ "--locale=C" "--encoding=UTF8" "--data-checksums" "--allow-group-access" ];
    # "warning: Currently nix-darwin does not support postgresql initialScript, ensureDatabases, or ensureUsers".

    # ensureDatabases = [ "haskbike" ];
    # ensureUsers = [
    #   {
    #     name = "haskbike";
    #     ensurePermissions = { "haskbike" = "ALL"; };
    #   }
    #   {
    #     name = "cfeeley";
    #     ensurePermissions = { "haskbike" = "ALL"; };
    #   }
    # ];
  };
}
