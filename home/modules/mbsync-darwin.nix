# SPDX-FileCopyrightText: Copyright (c) 2021 Calum MacRae
# SPDX-License-Identifier: MIT
#
## Source:
#
# https://github.com/LnL7/nix-darwin/pull/275
{ config, lib, pkgs, ... }:

with lib;

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  cfg = config.services.mbsync-darwin;

  mbsyncOptions = [ "--all" ] ++ optional (cfg.verbose) "--verbose"
    ++ optional (cfg.configFile != "")
    "--config ${cfg.configFile}";
in
{

  options.services.mbsync-darwin = {
    enable = mkEnableOption "mbsync";

    package = mkOption {
      type = types.package;
      default = pkgs.isync;
      defaultText = literalExample "pkgs.isync";
      example = literalExample "pkgs.isync";
      description = "The package to use for the mbsync binary.";
    };

    startInterval = mkOption {
      type = types.nullOr types.int;
      default = 300;
      example = literalExample "300";
      description = "Optional key to run mbsync every N seconds";
    };

    verbose = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether mbsync should produce verbose output.
      '';
    };

    configFile = mkOption {
      type = types.str;
      default = "";
      description = ''
        Optional configuration file to link to use instead of
        the default file (<filename>~/.mbsyncrc</filename>).
      '';
    };

    postExec = mkOption {
      type = types.str;
      default = "";
      example = "\${pkgs.mu}/bin/mu index";
      description = ''
        An optional command to run after mbsync executes successfully.
        This is useful for running mailbox indexing tools.
      '';
    };
  };

  config = mkIf (isDarwin && cfg.enable) {
    launchd.agents.mbsync = {
      enable = true;
      config = {
        Program = "${cfg.package}/bin/mbsync";
        ProgramArguments =
          [
            "${concatStringsSep " " mbsyncOptions}"
            (mkIf cfg.verbose "--verbose")
            "${optionalString (cfg.postExec != "") cfg.postExec}"
          ];
        RunAtLoad = true;
        KeepAlive = false;
        StartInterval = cfg.startInterval;
      };
    };
    home.activation.runMuInitDarwin = let
      maildirOption = genCmdMaildir config.accounts.email.maildirBasePath;
      dbLocation = config.xdg.cacheHome + "/mu";
  # Used to generate command line arguments that mu can operate with.
  genCmdMaildir = path: "--maildir=" + path;

  # Takes the list of accounts with mu.enable = true, and generates a
  # command-line flag for initializing the mu database.
  myAddresses = let
    # Set of email account sets where mu.enable = true.
    muAccounts =
      filter (a: a.mu.enable) (attrValues config.accounts.email.accounts);
    addrs = map (a: a.address) muAccounts;
    # Construct list of lists containing email aliases, and flatten
    aliases = flatten (map (a: a.aliases) muAccounts);
    # Prefix --my-address= to each account's address AND all defined aliases
    addMyAddress = map (addr: "--my-address=" + addr) (addrs ++ aliases);
  in concatStringsSep " " addMyAddress;
    in hm.dag.entryAfter [ "writeBoundary" ] ''
      # If the database directory exists, then `mu init` should NOT be run.
      # In theory, mu is the only thing that creates that directory, and it is
      # only created during the initial index.
      if [[ ! -d "${dbLocation}" ]]; then
        $DRY_RUN_CMD ${cfg.package}/bin/mbsync init ${maildirOption} ${myAddresses} $VERBOSE_ARG;
      fi
    '';
  };
}
