# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self, inputs, config, lib, pkgs, ... }:

# TODO: https://github.com/zhaofengli/attic/issues/114
let
  inherit (config.lib.dotfiles.secrets) secretsDir;

  cfg = config.services.cache;
  uploadCfg = config.nix.caches.attic.upload;

  attic-config-toml = {
    file = "${secretsDir}/attic-config.toml.age";
    owner = "cfeeley";
    group = "nixbld";
  };
in
{
  imports = [
    # Upstream attic module
    inputs.attic.nixosModules.atticd

    # Flake-local global module (sets services.cache options)
    self.collective.modules.global.cache

    # Cross-platform nix.caches options (substituters + upload toggle)
    self.collective.modules.global.nix.caches

    # Queued build hook
    inputs.queued-build-hook.nixosModules.queued-build-hook
  ];

  # nixpkgs >= 24.11 ships its own atticd module that collides with the
  # upstream one above. Suppress the bundled module; the upstream module is
  # the one the rest of this file targets.
  disabledModules = [ "services/networking/atticd.nix" ];

  options.services.cache = {
    enablePostgres = lib.mkEnableOption "Enable Cloudflare S3 storage";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
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
    })

    # Push built paths to the workstation attic cache via queued-build-hook.
    # Independent of `services.cache.enable` so non-server hosts (e.g.
    # proxmox-builder) can upload without running atticd themselves.
    (lib.mkIf uploadCfg.enable {
      age.secrets = { inherit attic-config-toml; };

      systemd.services.async-nix-post-build-hook.serviceConfig = {
        User = lib.mkForce "cfeeley";
        Group = lib.mkForce "cfeeley";
      };

      queued-build-hook = {
        enable = true;
        postBuildScriptContent =
          let
            uploadPathsScript = pkgs.writeShellApplication {
              name = "upload-paths";
              runtimeInputs = [ pkgs.attic pkgs.openssh ];
              text = ''
                set -eu
                set -f # disable globbing
                export IFS=' '

                # shellcheck disable=SC2086 # intentional word splitting
                exec ${pkgs.attic}/bin/attic push --config-path ${config.age.secrets.attic-config-toml.path} workstation:cfeeley $OUT_PATHS # $DRV_PATH
              '';
            };
          in
          "${uploadPathsScript}/bin/upload-paths";
      };
    })
  ];
}
