{ config, osConfig, options, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  cfg = config.services.gpg-agent;
  gpgPkg = config.programs.gpg.package;

  homedir = config.programs.gpg.homedir;

  gpgInitStr = ''
    GPG_TTY="$(tty)"
    export GPG_TTY
  '' + lib.optionalString cfg.enableSshSupport
    "${gpgPkg}/bin/gpg-connect-agent updatestartuptty /bye > /dev/null";

  # mimic `gpgconf` output for use in `systemd` unit definitions.
  # we cannot use `gpgconf` directly because it heavily depends on system
  # state, but we need the values at build time. original:
  # https://github.com/gpg/gnupg/blob/c6702d77d936b3e9d91b34d8fdee9599ab94ee1b/common/homedir.c#L672-L681
  sockRelPath = dir:
    let
      hash =
        lib.substring 0 24 (hexStringToBase32 (builtins.hashString "sha1" homedir));
    in
    if homedir == options.programs.gpg.homedir.default then
      "gnupg/${dir}"
    else
      "gnupg/d.${hash}/${dir}";
  gpgconf = dir: "%t/${sockRelPath dir}";
  gpgconf' = dir: "/tmp/${sockRelPath dir}";

  # Act like `xxd -r -p | base32` but with z-base-32 alphabet and no trailing padding.
  # Written in Nix for purity.
  hexStringToBase32 =
    let
      mod = a: b: a - a / b * b;
      pow2 = lib.elemAt [ 1 2 4 8 16 32 64 128 256 ];
      splitChars = s: lib.init (lib.tail (lib.splitString "" s));

      base32Alphabet = splitChars "ybndrfg8ejkmcpqxot1uwisza345h769";
      hexToIntTable = lib.listToAttrs (lib.genList
        (x: {
          name = lib.toLower (lib.toHexString x);
          value = x;
        }) 16);

      initState = {
        ret = "";
        buf = 0;
        bufBits = 0;
      };
      go = { ret, buf, bufBits }:
        hex:
        let
          buf' = buf * pow2 4 + hexToIntTable.${hex};
          bufBits' = bufBits + 4;
          extraBits = bufBits' - 5;
        in
        if bufBits >= 5 then {
          ret = ret + lib.elemAt base32Alphabet (buf' / pow2 extraBits);
          buf = mod buf' (pow2 extraBits);
          bufBits = bufBits' - 5;
        } else {
          ret = ret;
          buf = buf';
          bufBits = bufBits';
        };
    in
    hexString: (lib.foldl' go initState (splitChars hexString)).ret;

in
{
  meta.maintainers = [ lib.maintainers.rycee ];

  # Use our local fork of these modules while still pending upstream changes.
  # This is necessary in order to avoid tracking `nixpkgs-unstable` to appease hm.
  disabledModules = [ "services/gpg-agent.nix" ];

  options = {
    services.gpg-agent = {
      enable = lib.mkEnableOption "GnuPG private key agent";

      defaultCacheTtl = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = ''
          Set the time a cache entry is valid to the given number of
          seconds.
        '';
      };

      defaultCacheTtlSsh = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = ''
          Set the time a cache entry used for SSH keys is valid to the
          given number of seconds.
        '';
      };

      maxCacheTtl = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = ''
          Set the maximum time a cache entry is valid to n seconds. After this
          time a cache entry will be expired even if it has been accessed
          recently or has been set using gpg-preset-passphrase. The default is
          2 hours (7200 seconds).
        '';
      };

      maxCacheTtlSsh = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = ''
          Set the maximum time a cache entry used for SSH keys is valid to n
          seconds. After this time a cache entry will be expired even if it has
          been accessed recently or has been set using gpg-preset-passphrase.
          The default is 2 hours (7200 seconds).
        '';
      };

      enableSshSupport = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether to use the GnuPG key agent for SSH keys.
        '';
      };

      sshKeys = lib.mkOption {
        type = lib.types.nullOr (lib.types.listOf lib.types.str);
        default = null;
        description = ''
          Which GPG keys (by keygrip) to expose as SSH keys.
        '';
      };

      enableExtraSocket = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether to enable extra socket of the GnuPG key agent (useful for GPG
          Agent forwarding).
        '';
      };

      verbose = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether to produce verbose output.
        '';
      };

      grabKeyboardAndMouse = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Tell the pinentry to grab the keyboard and mouse. This
          option should in general be used to avoid X-sniffing
          attacks. When disabled, this option passes
          <option>no-grab</option> setting to gpg-agent.
        '';
      };

      allowPresetPassphrase = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          This option allows the use of <option>gpg-preset-passphrase</option> to seed the internal cache of gpg-agent with passphrases.
        '';
      };

      enableScDaemon = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Make use of the scdaemon tool. This option has the effect of
          enabling the ability to do smartcard operations. When
          disabled, this option passes
          <option>disable-scdaemon</option> setting to gpg-agent.
        '';
      };

      extraConfig = lib.mkOption {
        type = lib.types.lines;
        default = "";
        example = ''
          allow-loopback-pinentry
        '';
        description = ''
          Extra configuration lines to append to the gpg-agent
          configuration file.
        '';
      };

      pinentryFlavor = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum (pkgs.pinentry.flavors ++ [ "mac" "touchid" ]));
        example = "gnome3";
        default =
          if isDarwin
          then "mac"
          else "gtk2";
        description = ''
          Which pinentry interface to use. If not
          <literal>null</literal>, it sets
          <option>pinentry-program</option> in
          <filename>gpg-agent.conf</filename>. Beware that
          <literal>pinentry-gnome3</literal> may not work on non-Gnome
          systems. You can fix it by adding the following to your
          system configuration:
          <programlisting language="nix">
          services.dbus.packages = [ pkgs.gcr ];
          </programlisting>
          For this reason, the default is <literal>gtk2</literal> for
          now.
        '';
      };

      enableBashIntegration = lib.mkEnableOption "Bash integration" // {
        default = true;
      };

      enableZshIntegration = lib.mkEnableOption "Zsh integration" // {
        default = true;
      };

      enableFishIntegration = lib.mkEnableOption "Fish integration" // {
        default = true;
      };
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      home.file."${homedir}/gpg-agent.conf".text = (lib.concatStringsSep "\n" [
        (lib.optionalString (cfg.enableSshSupport) "enable-ssh-support")
        (if (!cfg.grabKeyboardAndMouse) then "no-grab" else "grab")
        (lib.optionalString (cfg.allowPresetPassphrase) "allow-preset-passphrase")
        (lib.optionalString (!cfg.enableScDaemon) "disable-scdaemon")
        (lib.optionalString (cfg.defaultCacheTtl != null) "default-cache-ttl ${toString cfg.defaultCacheTtl}")
        (lib.optionalString (cfg.defaultCacheTtlSsh != null) "default-cache-ttl-ssh ${toString cfg.defaultCacheTtlSsh}")
        (lib.optionalString (cfg.maxCacheTtl != null) "max-cache-ttl ${toString cfg.maxCacheTtl}")
        (lib.optionalString (cfg.maxCacheTtlSsh != null) "max-cache-ttl-ssh ${toString cfg.maxCacheTtlSsh}")
        (lib.optionalString (cfg.pinentryFlavor != null && cfg.pinentryFlavor != "mac" && cfg.pinentryFlavor != "touchid") "pinentry-program ${pkgs.pinentry.${cfg.pinentryFlavor}}/bin/pinentry")

        # NOTE: pinentry-touchid ALSO requires pinentry-program be 'pinentry-mac'
        (lib.optionalString (cfg.pinentryFlavor == "mac") "pinentry-program /opt/homebrew/bin/pinentry-mac")
        (lib.optionalString (cfg.pinentryFlavor == "touchid") "pinentry-program ${osConfig.homebrew.brewPrefix}/pinentry-touchid")
        "\n" # Trailing newline is important - avoids pinentry-program being jammed with the extraConfig.
      ]) + cfg.extraConfig;

      home.packages = lib.optionals (cfg.pinentryFlavor != null) (
        if isDarwin
        then [ pkgs.pinentry_mac ]
        else [ pkgs.pinentry.${cfg.pinentryFlavor} ]
      );

      programs.bash.initExtra = lib.mkIf cfg.enableBashIntegration gpgInitStr;
      programs.zsh.initExtra = lib.mkIf cfg.enableZshIntegration gpgInitStr;
      programs.fish.interactiveShellInit = lib.mkIf cfg.enableFishIntegration ''
        set -gx GPG_TTY (tty)
      '';

      # Trailing newlines are important
      home.activation.writeGpgSshControl = lib.mkIf (cfg.sshKeys != null) (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        echo "${(lib.concatMapStrings (s: '' ${s} '') cfg.sshKeys)}" > "${homedir}/.gnupg/sshcontrol"
      '');
    }

    # The systemd units below are direct translations of the
    # descriptions in the
    #
    #   ${gpgPkg}/share/doc/gnupg/examples/systemd-user
    #
    # directory.
    (lib.mkIf (pkgs.stdenv.hostPlatform.isLinux) (lib.mkMerge [
      {
        systemd.user.services.gpg-agent = {
          Unit = {
            Description = "GnuPG cryptographic agent and passphrase cache";
            Documentation = "man:gpg-agent(1)";
            Requires = "gpg-agent.socket";
            After = "gpg-agent.socket";
            # This is a socket-activated service:
            RefuseManualStart = true;
          };

          Service = {
            ExecStart = "${gpgPkg}/bin/gpg-agent --supervised"
              + lib.optionalString cfg.verbose " --verbose";
            ExecReload = "${gpgPkg}/bin/gpgconf --reload gpg-agent";
            Environment = [ "GNUPGHOME=${homedir}" ];
          };
        };

        systemd.user.sockets.gpg-agent = {
          Unit = {
            Description = "GnuPG cryptographic agent and passphrase cache";
            Documentation = "man:gpg-agent(1)";
          };

          Socket = {
            ListenStream = gpgconf "S.gpg-agent";
            FileDescriptorName = "std";
            SocketMode = "0600";
            DirectoryMode = "0700";
          };

          Install = { WantedBy = [ "sockets.target" ]; };
        };
      }

      (lib.mkIf cfg.enableSshSupport {
        systemd.user.sockets.gpg-agent-ssh = {
          Unit = {
            Description = "GnuPG cryptographic agent (ssh-agent emulation)";
            Documentation =
              "man:gpg-agent(1) man:ssh-add(1) man:ssh-agent(1) man:ssh(1)";
          };

          Socket = {
            ListenStream = gpgconf "S.gpg-agent.ssh";
            FileDescriptorName = "ssh";
            Service = "gpg-agent.service";
            SocketMode = "0600";
            DirectoryMode = "0700";
          };

          Install = { WantedBy = [ "sockets.target" ]; };
        };
      })

      (lib.mkIf cfg.enableExtraSocket {
        systemd.user.sockets.gpg-agent-extra = {
          Unit = {
            Description =
              "GnuPG cryptographic agent and passphrase cache (restricted)";
            Documentation = "man:gpg-agent(1) man:ssh(1)";
          };

          Socket = {
            ListenStream = gpgconf "S.gpg-agent.extra";
            FileDescriptorName = "extra";
            Service = "gpg-agent.service";
            SocketMode = "0600";
            DirectoryMode = "0700";
          };

          Install = { WantedBy = [ "sockets.target" ]; };
        };
      })
    ]))

    # Use GPG for SSH authorization
    {
      home.sessionVariables.SSH_AUTH_SOCK =
        "$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)";
    }

    (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin (lib.mkMerge [
      {
        launchd.agents.gpg-agent = {
          enable = true;
          config = {
            Program = "${gpgPkg}/bin/gpg-agent";
            ProgramArguments =
              [ "--supervised" (lib.mkIf cfg.verbose "--verbose") ];
            RunAtLoad = true;
            EnvironmentVariables = { GNUPGHOME = homedir; };
            KeepAlive.SuccessfulExit = false;
            Sockets.gpg-agent = {
              SockPathName = gpgconf' "S.gpg-agent";
              SockPathMode = 600;
              SockServiceName = "gpg-agent";
            };
          };
        };
      }
      (lib.mkIf cfg.enableSshSupport {
        launchd.agents.gpg-agent-ssh = {
          enable = true;
          config = {
            RunAtLoad = true;
            EnvironmentVariables = { GNUPGHOME = homedir; };
            KeepAlive.SuccessfulExit = false;
            Sockets.gpg-agent-ssh = {
              SockPathName = gpgconf' "S.gpg-agent.ssh";
              SockPathMode = 600;
              SockServiceName = "gpg-agent-ssh";
            };
          };
        };
      })
    ]))
  ]);
}
