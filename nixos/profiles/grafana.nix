{ self
, config
, lib
, pkgs
, ...
}:
let
  exporters = builtins.map
    (k: {
      inherit (k.config.services.prometheus.exporters.node) port;
      inherit (k.config.networking) hostName;
    })
    (lib.collect (k: k.services.prometheus.exporters.node.enable or false) self.nixosConfigurations);

  mkPrometheusTarget = { hostName, port }: {
    job_name = hostName;
    static_configs = [{
      targets = [ "${(lib.our.peers.getHost hostName).tailscale}:${port}" ];
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

    # Configure prometheus to read metrics from this exporter
    scrapeConfigs =
      # (lib.traceVal exporters) ++
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
