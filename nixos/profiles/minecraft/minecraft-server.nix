{ config
, lib
, pkgs
, profiles
, suites
, inputs
, primaryUser
, collective
, ...
}:
let
  inherit (config.lib.dotfield.secrets) mkAgeSecret;

  # inherit (collective) peers;
  # inherit (config.networking) hostName;
  # hostKeys = (lib.our.peers.getHost "MacBook-Pro").keys;
  # host = peers.hosts.${hostName};

  # "Borrowed" from AllTheMods Discord
  jvmOpts = lib.concatStringsSep " " [
    "-XX:+UseG1GC"
    "-XX:+ParallelRefProcEnabled"
    "-XX:MaxGCPauseMillis=200"
    "-XX:+UnlockExperimentalVMOptions"
    "-XX:+DisableExplicitGC"
    "-XX:+AlwaysPreTouch"
    "-XX:G1NewSizePercent=40"
    "-XX:G1MaxNewSizePercent=50"
    "-XX:G1HeapRegionSize=16M"
    "-XX:G1ReservePercent=15"
    "-XX:G1HeapWastePercent=5"
    "-XX:G1MixedGCCountTarget=4"
    "-XX:InitiatingHeapOccupancyPercent=20"
    "-XX:G1MixedGCLiveThresholdPercent=90"
    "-XX:G1RSetUpdatingPauseTimePercent=5"
    "-XX:SurvivorRatio=32"
    "-XX:+PerfDisableSharedMem"
    "-XX:MaxTenuringThreshold=1"
  ];

  # jvmOpts = lib.concatStringsSep " " [
  #   "-Xmx4G"
  #   "-XX:+UseConcMarkSweepGC"
  #   "-XX:+CMSIncrementalMode"
  #   "-XX:-UseAdaptiveSizePolicy"
  #   "-Xmn128M"
  # ];

  # Keys that can access the state of this instance (read/write!) over an rsync module
  # Leave empty to disable
  rsyncSSHKeys = primaryUser.authorizedKeys;

  defaults = {
    enable-rcon = true;
    # NOTE: insecure!
    # readFile is used here since the value should be read on the build machine,
    # and then written in plaintext into the instance's config file once deployed.
    # But most importantly, I don't really care if this password is exposed.
    rcon-password = config.age.secrets."minecraft-rcon-password.txt".path;

    # Only people in the Cool Club (tm)
    white-list = true;

    # So I don't have to make everyone op
    spawn-protection = 0;

    # 5 minutes tick timeout, for heavy packs
    max-tick-time = 5 * 60 * 1000;

    # It just ain't modded minecraft without flying around
    allow-flight = true;
  };
in
{
  imports = [ inputs.modded-minecraft-servers.module ];

  config = {
    age.secrets = lib.mkMerge [
      (mkAgeSecret "minecraft-rcon-password.txt")
    ];
    environment.systemPackages = with pkgs; [
      ferium # <- CLI program for managing Minecraft modpacks from Modrinth, CurseForge, and Github Releases
      mcrcon # <- Minecraft console client
    ];

    services.modded-minecraft-servers = {
      # Of course I read the totally legally-binding EULA.
      eula = true;

      instances = {
        # The name will be used for the state folder and system user.
        # In this case, the folder is `/var/lib/mc-rlcraft`
        # and the user `mc-rlcraft`.
        rlcraft = {
          inherit rsyncSSHKeys jvmOpts;

          # Call 'start.sh' in '/var/lib/mc-rlcraft'. $JVMOPTS will be set appropriately.
          enable = false;

          jvmMaxAllocation = "4G";
          jvmInitialAllocation = "2G";

          serverConfig = defaults // {
            # Port must be unique
            server-port = 25566;
            rcon-port = 25575;
            motd = "Welcome to RLCraft";

            # enable-command-block MUST be TRUE for villagers to spawn correctly in generated structures.
            enable-command-block = true;
            # allow flight needs to be true for things like mounts that fly and fairy ring, or the server will kick players using those.
            allow-flight = true;
            # difficulty needs to be 3 for hard difficulty mode and the difficulty RLCraft is balanced around.
            difficulty = 1; # FIXME: laaaame..
            # max-tick-time need to be -1 for big structures that generate (AND FOR PREGENNING AS WELL) as these structures take take some time to generate, and this prevents forge from thinking the server crashed and auto shutting it down.
            max-tick-time = -1;
            # view-distance should be 10 for full compatibility, but if you get performance issues with many players, lower this to 6
            view-distance = 10;
          };

          # Setup commands:
          # ferium profile create
          # ferium modpack add 285109
          # ferium modpack upgrade
          #
          # Start script:
          # exec java -server "${JVMOPTS[@]}" -jar forge-1.12.2-14.23.5.2847-universal.jar nogui

          # Install server (run from client):
          # nix run nixpkgs#jre8 -- java -jar forge-1.12.2-14.23.5.2860-installer.jar --installServer
        };

        atm7 = {
          inherit rsyncSSHKeys jvmOpts;

          # Call 'start.sh' in '/var/lib/mc-rlcraft'. $JVMOPTS will be set appropriately.
          enable = true;

          jvmPackage = pkgs.jdk17;
          jvmMaxAllocation = "4G";
          jvmInitialAllocation = "2G";

          serverConfig = defaults // {
            # Port must be unique
            server-port = 25566;
            rcon-port = 25575;
            motd = "atm7";

            # enable-command-block MUST be TRUE for villagers to spawn correctly in generated structures.
            enable-command-block = true;
            # allow flight needs to be true for things like mounts that fly and fairy ring, or the server will kick players using those.
            allow-flight = true;
            # difficulty needs to be 3 for hard difficulty mode and the difficulty RLCraft is balanced around.
            difficulty = 1; # FIXME: laaaame..
            # max-tick-time need to be -1 for big structures that generate (AND FOR PREGENNING AS WELL) as these structures take take some time to generate, and this prevents forge from thinking the server crashed and auto shutting it down.
            max-tick-time = -1;
            # view-distance should be 10 for full compatibility, but if you get performance issues with many players, lower this to 6
            view-distance = 10;
          };

          # Setup commands:
          # ferium profile create
          # ferium modpack add 285109
          # ferium modpack upgrade
          #
          # Start script:
          # exec java -server "${JVMOPTS[@]}" -jar forge-1.12.2-14.23.5.2847-universal.jar nogui

          # Install server (run from client):
          # nix run nixpkgs#jre8 -- java -jar forge-1.12.2-14.23.5.2860-installer.jar --installServer
        };
      };
    };
  };
}
