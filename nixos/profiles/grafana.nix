{ self
, config
, lib
, pkgs
, ...
}:
let
  exporters = lib.mapAttrsToList
    (k: v: mkPrometheusTarget {
      inherit (v.config.services.prometheus.exporters.node) port;
      inherit (v.config.networking) hostName;
    })
    (lib.filterAttrs (k: v: v.config.services.prometheus.exporters.node.enable) (self.nixosConfigurations));

  mkPrometheusTarget = { hostName, port }: {
    job_name = hostName;
    static_configs = [{
      targets = [ "${(lib.our.peers.getHost hostName).tailscale}:${toString port}" ];
    }];
  };
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
    };

    scrapeConfigs =
      # Read metrics from all hosts in nixosConfigurations with the prometheus node exporter enabled
      exporters ++
      # Configure prometheus to read metrics from this exporter
      [
        {
          job_name = config.networking.hostName;
          static_configs = [{
            targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
          }];
        }
      ];
  };
}
