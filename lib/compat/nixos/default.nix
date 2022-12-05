_:
let
  default = (import ../.).defaultNix;
  configs = default.nixosConfigurations;
  host = configs.${hostname} or configs.workstation;
  hostname = builtins.getEnv "HOSTNAME";
in
host
