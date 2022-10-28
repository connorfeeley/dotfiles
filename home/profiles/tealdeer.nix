{ config
, lib
, pkgs
, ...
}: {
  programs.tealdeer = {
    enable = true;
    settings = {
      cache_dir = "${config.xdg.configHome}/tealdeer";
      display = {
        compact = false;
        use_pager = false;
      };

      updates = {
        auto_update = true;
        auto_update_interval_hours = 168; # 1 week
      };
    };
  };

  home.activation.ensureTealdeerCacheDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "${config.programs.tealdeer.settings.cache_dir}"
  '';
}
