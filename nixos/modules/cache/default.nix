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
    group = "nixbld";
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

    queued-build-hook = {
      enable = true;
      postBuildScriptContent = ''
        set -eu
        set -f # disable globbing
        export IFS=' '


        export HOME="$RUNTIME_DIRECTORY"
        ls -al $HOME
        cat ${config.age.secrets.attic-config-toml.path}


        echo "Writing attic config"
        mkdir -p $HOME/.config/attic
        cp ${config.age.secrets.attic-config-toml.path} $HOME/.config/attic/config.toml

        echo "Uploading paths to attic" $OUT_PATHS
        echo "Uploading derivations to attic" $DRV_PATHS
        exec ${pkgs.attic}/bin/attic push workstation:cfeeley $OUT_PATHS $DRV_PATH
      '';
    };

    systemd.services.async-nix-post-build-hook.serviceConfig = {
      Group = lib.mkForce "nixbld";
      RuntimeDirectory = "async-nix-post-build-hook";
      RuntimeDirectoryMode = "0700";
      WorkingDirectory = "%t/async-nix-post-build-hook";
    };
  };
}
