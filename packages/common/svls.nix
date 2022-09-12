{ lib
, rustPlatform
, fetchFromGitHub
}:

rustPlatform.buildRustPackage rec {
  pname = "svls";
  version = "v0.2.5";

  src = fetchFromGitHub {
    owner = "dalance";
    repo = pname;
    rev = version;
    sha256 = "sha256-SeVLQ05vPywSOnOEhJhQXYhdptmIhvYbbf9SX5eVzik=";
  };

  cargoSha256 = "sha256-CFT/Oex5kIyqg/rTDdYkE1QrG6zG+S+TPOZo6KJHkhI=";

  meta = with lib; {
    description = "SystemVerilog language server";
    homepage = "https://github.com/dalance/svls";
    license = licenses.mit;
    maintainers = [ maintainers.cfeeley ];
  };
}
