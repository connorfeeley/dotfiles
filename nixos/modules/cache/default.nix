# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self, inputs, config, lib, pkgs, ... }:

# TODO: https://github.com/zhaofengli/attic/issues/114
let cfg = config.services.cache;
in {
  imports = [
    # Upstream attic module
    inputs.attic.nixosModules.atticd
    # Flake-local global module (sets options)
    self.collective.modules.global.cache
  ];

  options.services.cache = {
    enable = lib.mkEnableOption "Serve an S3-backed Nix cache using attic";
    enableCloudflareS3 = lib.mkEnableOption "Enable Cloudflare S3 storage";
    enablePostgres = lib.mkEnableOption "Enable Cloudflare S3 storage";
  };

  config = lib.mkIf cfg.enable {
    services.postgresql = lib.mkIf cfg.enablePostgres {
      # Configure a PostgreSQL database for attic
      enable = true;
      ensureDatabases = [ "attic" ];
      ensureUsers = [{
        name = "attic";
        ensureDBOwnership = true;
      }];
    };

    # Reverse proxy port 9090 to https://workstation.elephant-vibes.ts.net/services/cache
    services.nginx = {
      enable = true;
      virtualHosts."workstation.elephant-vibes.ts.net" = {
        locations."/services/cache/" = {
          proxyPass = "http://localhost:9090/";
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
  };
}
