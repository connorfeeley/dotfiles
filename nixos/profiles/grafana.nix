{ self
, config
, lib
, ...
}:
let
  inherit (lib) filterAttrs mapAttrs attrValues flatten;

  allHosts = self.nixosConfigurations;

  enabledExportersF = name: host: filterAttrs (k: v: v.enable == true) host.config.services.prometheus.exporters;
  enabledExporters = mapAttrs enabledExportersF allHosts;

  mkScrapeConfigExporter = hostname: ename: ecfg: {
    job_name = "${hostname}-${ename}";
    static_configs = [{ targets = [ "${hostname}:${toString ecfg.port}" ]; }];
    relabel_configs = [
      {
        target_label = "instance";
        replacement = "${hostname}";
      }
      {
        target_label = "job";
        replacement = "${ename}";
      }
    ];
  };

  mkScrapeConfigHost = name: exporters: mapAttrs (mkScrapeConfigExporter name) exporters;
  scrapeConfigsByHost = mapAttrs mkScrapeConfigHost enabledExporters;

  autogenScrapeConfigs = flatten (map attrValues (attrValues scrapeConfigsByHost));
in
{
  services.grafana = {
    enable = true;
    settings.server = {
      # Listening address and TCP port
      http_addr = "0.0.0.0";
      http_port = 9010;
      # Grafana needs to know on which domain and URL it's running:
      domain = config.networking.hostName;
    };
    provision = {
      enable = true;
      # Set up the datasources
      datasources.settings.datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          access = "proxy";
          url = "http://localhost:${toString config.services.prometheus.port}";
          isDefault = true;
        }
      ];
    };
  };
  services.prometheus = {
    # https://xeiaso.net/blog/prometheus-grafana-loki-nixos-2020-11-20
    enable = true;
    port = 9011;

    # Enable node exporter
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9012;
      };
      smartctl = {
        enable = config.services.smartd.enable;
        openFirewall = config.services.smartd.enable;
        # Defaults:
        user = "smartctl-exporter";
        group = "disk";
        port = 9633;
      };
    };

    scrapeConfigs = autogenScrapeConfigs ++
      [
        (
          let
            hostname = "h8tsner";
            ename = "endlessh-go";
            ecfg.port = 2112;
          in
          {
            job_name = "${hostname}-${ename}";
            static_configs = [{ targets = [ "${hostname}:${toString ecfg.port}" ]; }];
            relabel_configs = [
              {
                target_label = "instance";
                replacement = "${hostname}";
              }
              {
                target_label = "job";
                replacement = "${ename}";
              }
            ];
          }
        )
      ];
  };
}
