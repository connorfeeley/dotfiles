# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self, inputs, config, lib, pkgs, ... }:

# TODO: https://github.com/zhaofengli/attic/issues/114
let
  inherit (config.lib.dotfield.secrets) secretsDir;

  cfg = config.services.cache;

  attic-config-toml = {
    file = "${secretsDir}/attic-config.toml.age";
    owner = "cfeeley";
  };
in
{
  imports = [
    # Upstream attic module
    inputs.attic.nixosModules.atticd

    # Flake-local global module (sets options)
    self.collective.modules.global.cache

    # Queued build hook
    inputs.queued-build-hook.nixosModules.queued-build-hook
  ];

  options.services.cache = {
    enablePostgres = lib.mkEnableOption "Enable Cloudflare S3 storage";
  };

  config = lib.mkIf cfg.enable {

    environment.systemPackages = [ pkgs.attic ];
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
        locations."/cache" = {
          extraConfig = "return 302 /cache/;";
        };
        locations."/" = {
          proxyPass = "http://[::]:9090";
          proxyWebsockets = true; # needed if you need to use WebSocket
          extraConfig =
            # "rewrite ^/services/cache(.*) /$1 break;" + # remove the /services/cache prefix
            "client_max_body_size 10G;" +
            # required when the target is also TLS server with multiple hosts
            "proxy_ssl_server_name on;" +
            # required when the server wants to use HTTP Authentication
            "proxy_pass_header Authorization;"
          ;
        };
      };
    };

    age.secrets = { inherit attic-config-toml; };

    # Can test post-build-hook with:
    # nix-build --expr '(import <nixpkgs> {}).writeText "example" (builtins.toString builtins.currentTime)'
  #   queued-build-hook = {
  #     enable = true;
  #     postBuildScript = pkgs.writeShellScript "hook" ''
  #       set -eu
  #       set -f # disable globbing
  #       export IFS=' '

  #       set -x

  #       echo "Uploading paths to attic" $OUT_PATHS
  #       echo "Uploading derivations to attic" $DRV_PATH
  #       exec ${pkgs.attic}/bin/attic push workstation:cfeeley "$OUT_PATHS" "$DRV_PATH"
  #     '';
  #   };

  #   systemd.services.async-nix-post-build-hook.environment = {
  #     LANG = "en_US.UTF-8";
  #     LC_ALL = "en_US.UTF-8";
  #   };
  #   systemd.services.async-nix-post-build-hook.serviceConfig = {
  #     # StandardOutput = "file:/var/log/async-nix-post-build-hook.log";
  #     # Environment = "LANGUAGE=en_US.UTF-8";
  #     # DynamicUser = lib.mkForce false;
  #     User = lib.mkForce "cfeeley";
  #     # Group = lib.mkForce "cfeeley";
  #     # LoadCredential = null;
  #     # RuntimeDirectory = "async-nix-post-build-hook";
  #     # RuntimeDirectoryMode = "0700";
  #     # WorkingDirectory = "%t/async-nix-post-build-hook";
  #   };
  };
}
