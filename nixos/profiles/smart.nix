{ config
, ...
}:
{
  services.smartd = {
    enable = true;
    autodetect = true;
  };
}
