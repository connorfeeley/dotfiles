{ inputs, config, pkgs, lib, ... }:
{
  services.hercules-ci-agent.enable = true;

  environment.systemPackages = [ pkgs.hci ];
}
