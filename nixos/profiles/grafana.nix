{ self, config, lib, ... }:
let
  inherit (lib) filterAttrs mapAttrs attrValues flatten;

  allHosts = self.nixosConfigurations;

  enabledExportersF = name: host: filterAttrs (k: v: lib.isBool v && v.enable) host.config.services.prometheus.exporters;
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

  mkScrapeConfigHost = name: exporters:
    mapAttrs (mkScrapeConfigExporter name) exporters;
  scrapeConfigsByHost = mapAttrs mkScrapeConfigHost enabledExporters;

  autogenScrapeConfigs =
    flatten (map attrValues (attrValues scrapeConfigsByHost));

  lokiPort = 3100;
  prometheusPort = 9090;
  prometheusNodePort = 9100;

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
      datasources.settings.datasources = [{
        name = "Prometheus";
        type = "prometheus";
        access = "proxy";
        url = "http://localhost:${toString config.services.prometheus.port}";
        isDefault = true;
      }];
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
      postgres.enable = true;
      zfs.enable = true;
    };

    scrapeConfigs = autogenScrapeConfigs ++ [
      {
        job_name = "workstation";
        static_configs =
          [
            { targets = [ "workstation:${toString config.services.prometheus.exporters.node.port}" ]; }
            { targets = [ "workstation:${toString config.services.prometheus.exporters.smartctl.port}" ]; }
            { targets = [ "workstation:${toString config.services.prometheus.exporters.postgres.port}" ]; }
            { targets = [ "workstation:${toString config.services.prometheus.exporters.zfs.port}" ]; }
          ];
      }
    ];
  };

  services.loki = {
    enable = true;
    configuration = {
      auth_enabled = false;
      server = {
        http_listen_port = lokiPort;
        log_level = "warn";
      };
      ingester = {
        lifecycler = {
          address = "127.0.0.1";
          ring = {
            kvstore.store = "inmemory";
            replication_factor = 1;
          };
          final_sleep = "0s";
        };
        chunk_idle_period = "5m";
        chunk_retain_period = "30s";
      };
      schema_config = {
        configs = [
          {
            from = "2022-05-06";
            store = "boltdb";
            object_store = "filesystem";
            schema = "v11";
            index = {
              prefix = "index_";
              period = "48h";
            };
          }
        ];
      };
      storage_config = {
        boltdb.directory = "/tmp/loki/index";
        filesystem.directory = "/tmp/loki/chunks";
      };
      limits_config = {
        enforce_metric_name = false;
        reject_old_samples = true;
        reject_old_samples_max_age = "168h";
      };
      # ruler = {
      #   alertmanager_url = "http://localhost:9093";
      # };
      analytics = {
        reporting_enabled = false;
      };
    };
  };

  services.promtail = {
    enable = true;
    configuration = {
      server = {
        http_listen_port = 28183;
        grpc_listen_port = 0;
        log_level = "warn";
      };
      positions.filename = "/tmp/positions.yaml";
      clients = [
        { url = "http://127.0.0.1:${toString lokiPort}/loki/api/v1/push"; }
      ];
      scrape_configs = [
        {
          job_name = "journal";
          journal = {
            max_age = "24h";
            labels = {
              job = "systemd-journal";
              host = "127.0.0.1";
            };
          };
          relabel_configs = [
            {
              source_labels = [ "__journal__systemd_unit" ];
              target_label = "unit";
            }
          ];
        }
      ];
    };
  };
}
