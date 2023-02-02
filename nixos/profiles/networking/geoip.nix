{...}: {
  services.geoipupdate = {
    enable = true;
    settings = {
      AccountID = 821562;
      LicenseKey = { _secret = "/etc/secrets/maxmind_license_key"; };
      EditionIDs =  [ "GeoLite2-ASN" "GeoLite2-City" "GeoLite2-Country" ];
      DatabaseDirectory = "/var/lib/GeoIP";
    };
  };
}
