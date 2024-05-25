# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.services.cache;
in
{
  imports = [ inputs.attic.nixosModules.atticd ];

  options.services.cache.enable = lib.mkEnableOption "Serve an S3-backed Nix cache using attic";
  options.services.cache.enableCloudflareS3 = lib.mkEnableOption "Enable Cloudflare S3 storage";

  config = lib.mkIf cfg.enable {
    services.atticd = {
      enable = true;

      # Contains:
      # ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64="output of: <openssl rand 64 | base64 -w0>"
      # AWS_ACCESS_KEY_ID="<cloudflare access key>"
      # AWS_SECRET_ACCESS_KEY="<cloudflare secret key>"
      credentialsFile = "/etc/secrets/atticd.env";

      settings = {
        listen = "[::]:9090";

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
      };
    };

    # Reverse proxy port 9090 to https://workstation.elephant-vibes.ts.net/services/cache
    services.nginx.enable = true;
    services.nginx.virtualHosts."workstation.elephant-vibes.ts.net" = {
      locations."/services/cache" = {
        proxyPass = "http://localhost:9090";
        proxyWebsockets = true; # needed if you need to use WebSocket
        extraConfig =
          "client_max_body_size 10G;" +
          # required when the target is also TLS server with multiple hosts
          "proxy_ssl_server_name on;" +
          # required when the server wants to use HTTP Authentication
          "proxy_pass_header Authorization;"
        ;
      };
    };
  };
}
