# ########################################################################
#  ____                                _           _                    #
# |  _ \  ___ _ __  _ __ ___  ___ __ _| |_ ___  __| |                   #
# | | | |/ _ \ '_ \| '__/ _ \/ __/ _` | __/ _ \/ _` |  _____            #
# | |_| |  __/ |_) | | |  __/ (_| (_| | ||  __/ (_| | |_____|           #
# |____/ \___| .__/|_|  \___|\___\__,_|\__\___|\__,_|                   #
#            |_|                                                        #
#                   _ _     _               _            _              #
#   __ ___   ____ _(_) |   (_)_ __    _ __ (_)_  ___ __ | | ____ _ ___  #
#  / _` \ \ / / _` | | |   | | '_ \  | '_ \| \ \/ / '_ \| |/ / _` / __| #
# | (_| |\ V / (_| | | |_  | | | | | | | | | |>  <| |_) |   < (_| \__ \ #
#  \__,_| \_/ \__,_|_|_(_) |_|_| |_| |_| |_|_/_/\_\ .__/|_|\_\__, |___/ #
#                                                 |_|        |___/      #
#########################################################################

{ lib, rustPlatform, fetchFromGitHub }:

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
