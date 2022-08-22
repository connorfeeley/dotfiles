{
  config,
  lib,
  pkgs,
  ...
}: {
  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluezFull;

  # Fix: bluetooth suspend causes short sounds to be missed (ex: notifications).
  # Configure wireplumber not to suspend bluetooth devices.
  environment.etc = {
    "wireplumber/main.lua.d/51-suspend.lua".text = ''
      alsa_monitor.rules = {
        {
          matches = {
            {
              -- Matches all sources.
              --{ "node.name", "matches", "alsa_input.*" },
            },
          },
          apply_properties = {
            --["node.nick"]              = "My Node",
            --["priority.driver"]        = 100,
            --["priority.session"]       = 100,
            --["node.pause-on-idle"]     = false,
            --["resample.quality"]       = 4,
            --["channelmix.normalize"]   = false,
            --["channelmix.mix-lfe"]     = false,
            --["audio.channels"]         = 2,
            --["audio.format"]           = "S16LE",
            --["audio.rate"]             = 44100,
            --["audio.allowed-rates"]    = "32000,96000"
            --["audio.position"]         = "FL,FR",
            --["api.alsa.period-size"]   = 1024,
            --["api.alsa.headroom"]      = 0,
            --["api.alsa.disable-mmap"]  = false,
            --["api.alsa.disable-batch"] = false,
            ["session.suspend-timeout-seconds"] = 0,  -- 0 disables suspend
          },
        },
        {
          matches = {
            {
              -- Matches all sources.
              { "node.name", "matches", "alsa_input.*" },
            },
          },
          apply_properties = {
            --["node.nick"]              = "My Node",
            --["priority.driver"]        = 100,
            --["priority.session"]       = 100,
            --["node.pause-on-idle"]     = false,
            --["resample.quality"]       = 4,
            --["channelmix.normalize"]   = false,
            --["channelmix.mix-lfe"]     = false,
            --["audio.channels"]         = 2,
            --["audio.format"]           = "S16LE",
            --["audio.rate"]             = 44100,
            --["audio.allowed-rates"]    = "32000,96000"
            --["audio.position"]         = "FL,FR",
            --["api.alsa.period-size"]   = 1024,
            --["api.alsa.headroom"]      = 0,
            --["api.alsa.disable-mmap"]  = false,
            --["api.alsa.disable-batch"] = false,
            ["session.suspend-timeout-seconds"] = 5,  -- 0 disables suspend
          },
        },
      }
    '';
  };
}
