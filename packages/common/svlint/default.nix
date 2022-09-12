{ lib
, rustPlatform
, fetchFromGitHub
}:

rustPlatform.buildRustPackage rec {
  pname = "svlint";
  version = "v0.5.6";

  src = fetchFromGitHub {
    owner = "dalance";
    repo = pname;
    rev = version;
    sha256 = "sha256-CAyFW+jTOLCQuHIYjdp9vujLIIjL2FXlCLsQiUyJ22o=";
  };

  cargoPatches = [
    ./0001-feat-generate-lockfile.patch
  ];

  cargoSha256 = "sha256-uCYTYyzOShaTqJQwGb6DCX5zpzANLJ0/771Vo8lTQcQ=";

  meta = with lib; {
    description = "SystemVerilog linter";
    homepage = "https://github.com/dalance/svlint";
    license = licenses.mit;
    maintainers = [ maintainers.cfeeley ];
  };
}
