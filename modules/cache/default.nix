# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, lib, ... }:

let
  inherit (config.lib.dotfiles.secrets) secretsDir;

  cfg = config.services.cache;

  attic-cache-secrets = {
    file = "${secretsDir}/hosts/workstation/attic-cache-secrets.env.age";
    owner = config.services.atticd.user;
  };
in
{
  options.services.cache = {
    enable = lib.mkEnableOption "Serve an S3-backed Nix cache using attic";
    enableCloudflareS3 = lib.mkEnableOption "Enable Cloudflare S3 storage";
  };

  config = lib.mkIf cfg.enable {
    age.secrets = { inherit attic-cache-secrets; };

    services.atticd = {
      enable = true;

      # Contains:
      # ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64="output of: <openssl rand 64 | base64 -w0>"
      # AWS_ACCESS_KEY_ID="<cloudflare access key>"
      # AWS_SECRET_ACCESS_KEY="<cloudflare secret key>"
      credentialsFile = config.age.secrets.attic-cache-secrets.path;

      settings = {
        listen = "[::]:9090";

        # Default: "sqlite:///var/lib/atticd/server.db?mode=rwc"
        database.url = lib.mkIf cfg.enablePostgres "postgresql://attic@localhost/attic";

        # Data chunking
        #
        # Warning: If you change any of the values here, it will be
        # difficult to reuse existing chunks for newly-uploaded NARs
        # since the cutpoints will be different. As a result, the
        # deduplication ratio will suffer for a while after the change.
        chunking = {
          # The minimum NAR size to trigger chunking
          #
          # If 0, chunking is disabled entirely for newly-uploaded NARs.
          # If 1, all NARs are chunked.
          nar-size-threshold = 64 * 1024; # 64 KiB

          # The preferred minimum size of a chunk, in bytes
          min-size = 16 * 1024; # 16 KiB

          # The preferred average size of a chunk, in bytes
          avg-size = 64 * 1024; # 64 KiB

          # The preferred maximum size of a chunk, in bytes
          max-size = 256 * 1024; # 256 KiB
        };

        # Storage configuration
        storage = lib.mkIf cfg.enableCloudflareS3 {
          type = "s3";
          bucket = "cfeeley-nixpkgs-cache";
          region = "auto";
          endpoint = "https://58f2f5e716707689e3cf7b16c1efcf28.r2.cloudflarestorage.com";
        };

        # Garbage collection
        garbage-collection = {
          interval = "12 hours"; # Default value
          # Retention period
          default-retention-period = "6 months"; # Default value
        };
      };
    };
  };
}
