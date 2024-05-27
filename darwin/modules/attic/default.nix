{ config, lib, pkgs, ... }:

let
  inherit (lib) types;

  cfg = config.services.atticd;

  format = pkgs.formats.toml { };

  checkedConfigFile = pkgs.runCommand "checked-attic-server.toml"
    {
      configFile = cfg.configFile;
    } ''
    cat $configFile

    export ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64="dGVzdCBzZWNyZXQ="
    export ATTIC_SERVER_DATABASE_URL="sqlite://:memory:"
    ${cfg.package}/bin/atticd --mode check-config -f $configFile
    cat <$configFile >$out
  '';

  atticadmShim = pkgs.writeShellScript "atticadm" ''
    if [ -n "$ATTICADM_PWD" ]; then
      cd "$ATTICADM_PWD"
      if [ "$?" != "0" ]; then
        >&2 echo "Warning: Failed to change directory to $ATTICADM_PWD"
      fi
    fi

    exec ${cfg.package}/bin/atticadm -f ${checkedConfigFile} "$@"
  '';

  atticadmWrapper = pkgs.writeShellScriptBin "atticd-atticadm" ''
    export ATTICADM_PWD=$(pwd)
    su -m ${cfg.user} -c "${atticadmShim} $@"
  '';

  hasLocalPostgresDB =
    let
      url = cfg.settings.database.url or "";
      localStrings = [ "localhost" "127.0.0.1" "/run/postgresql" ];
      hasLocalStrings = lib.any (lib.flip lib.hasInfix url) localStrings;
    in
    config.services.postgresql.enable && lib.hasPrefix "postgresql://" url && hasLocalStrings;
in
{
  options = {
    services.atticd = {
      enable = lib.mkOption {
        description = ''
          Whether to enable the atticd, the Nix Binary Cache server.
        '';
        type = types.bool;
        default = false;
      };
      package = lib.mkOption {
        description = ''
          The package to use.
        '';
        type = types.package;
        default = pkgs.attic-server;
      };
      credentialsFile = lib.mkOption {
        description = ''
          Path to an EnvironmentFile containing required environment
          variables:

          - ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64: The Base64-encoded version of the
            HS256 JWT secret. Generate it with `openssl rand 64 | base64 -w0`.
        '';
        type = types.nullOr types.path;
        default = null;
      };
      user = lib.mkOption {
        description = ''
          The group under which attic runs.
        '';
        type = types.str;
        default = "atticd";
      };
      group = lib.mkOption {
        description = ''
          The user under which attic runs.
        '';
        type = types.str;
        default = "atticd";
      };
      settings = lib.mkOption {
        description = ''
          Structured configurations of atticd.
        '';
        type = format.type;
        default = { }; # setting defaults here does not compose well
      };
      configFile = lib.mkOption {
        description = ''
          Path to an existing atticd configuration file.

          By default, it's generated from `services.atticd.settings`.
        '';
        type = types.path;
        default = format.generate "server.toml" cfg.settings;
        defaultText = "generated from `services.atticd.settings`";
      };

      mode = lib.mkOption {
        description = ''
          Mode in which to run the server.

          'monolithic' runs all components, and is suitable for single-node deployments.

          'api-server' runs only the API server, and is suitable for clustering.

          'garbage-collector' only runs the garbage collector periodically.

          A simple NixOS-based Attic deployment will typically have one 'monolithic' and any number of 'api-server' nodes.

          There are several other supported modes that perform one-off operations, but these are the only ones that make sense to run via the NixOS module.
        '';
        type = lib.types.enum [ "monolithic" "api-server" "garbage-collector" ];
        default = "monolithic";
      };
      workingDirectory = lib.mkOption {
        type = types.str;
        default = "/var/lib/atticd";
        description = ''
          The working directory of the atticd daemon process.
        '';
      };
    };
  };
  config = lib.mkIf (cfg.enable) (lib.mkMerge [
    {
      assertions = [
        {
          assertion = cfg.credentialsFile != null;
          message = ''
            <option>services.atticd.credentialsFile</option> is not set.

            Run `openssl rand 64 | base64 -w0` and create a file with the following contents:

            ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64="output from command"

            Then, set `services.atticd.credentialsFile` to the quoted absolute path of the file.
          '';
        }
        {
          assertion = !lib.isStorePath cfg.credentialsFile;
          message = ''
            <option>services.atticd.credentialsFile</option> points to a path in the Nix store. The Nix store is globally readable.

            You should use a quoted absolute path to prevent this.
          '';
        }
      ];

      services.atticd.settings = {
        database.url = lib.mkDefault "sqlite:///var/lib/atticd/server.db?mode=rwc";

        # "storage" is internally tagged
        # if the user sets something the whole thing must be replaced
        storage = lib.mkDefault {
          type = "local";
          path = "/var/lib/atticd/storage";
        };
      };

      launchd.daemons.atticd = {
        command = "";
        script = ''
          source ${cfg.environmentFile}"
          ${cfg.package}/bin/atticd -f ${checkedConfigFile} --mode ${cfg.mode}
        '';
        serviceConfig = {
          WorkingDirectory = cfg.workingDirectory;
          ThrottleInterval = 10;
          # RunAtLoad = false; # TODO delete?
          # StartCalendarInterval = [ cfg.interval ];
          UserName = cfg.user;
          GroupName = cfg.group;
        };
      };

      environment.systemPackages = [ atticadmWrapper ];

      # Create working directory
      system.activationScripts.preActivation.text = ''
        mkdir -p ${cfg.workingDirectory}
      '';

    }
  ]);
}
