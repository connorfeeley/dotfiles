{ inputs, config, pkgs, lib, ... }: {
  services.hercules-ci-agent.enable = true;
  services.hercules-ci-agent.settings = {
    labels.tags = [ pkgs.stdenv.system ];
  };

  environment.systemPackages = [ pkgs.hci ];
}
