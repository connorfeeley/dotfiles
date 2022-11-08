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

  # Keys that can access the state of this instance (read/write!) over an rsync module
  # Leave empty to disable
  rsyncSSHKeys = primaryUser.authorizedKeys;

  defaults = {
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
    environment.systemPackages = with pkgs; [
      # CLI program for managing Minecraft modpacks from Modrinth, CurseForge, and Github Releases
      ferium
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
          enable = true;

          jvmMaxAllocation = "8G";
          jvmInitialAllocation = "2G";

          serverConfig = defaults // {
            # Port must be unique
            server-port = 25566;
            motd = "Welcome to RLCraft";

            enable-command-block = true;
          };
        };
      };
    };
  };
}
