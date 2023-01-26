{ lib
, python3
, fetchFromGitHub
}:

python3.pkgs.buildPythonApplication rec {
  pname = "aranet4";
  version = "2.1.3";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "Anrijs";
    repo = "Aranet4-Python";
    rev = "v${version}";
    hash = "sha256-5q4eOC9iuN8pUmDsiQ7OwEXkxi4KdL+bhGVjlQlTBAg=";
  };

  propagatedBuildInputs = with python3.pkgs; [
    bleak
    requests
  ];

  pythonImportsCheck = [ "aranet4" ];

  meta = with lib; {
    description = "Aranet4 Python client";
    homepage = "https://github.com/Anrijs/Aranet4-Python/archive/refs/tags/v2.1.3.tar.gz";
    license = licenses.mit;
    platforms = lib.platforms.linux;
    maintainers = with maintainers; [ cfeeley ];
  };
}
