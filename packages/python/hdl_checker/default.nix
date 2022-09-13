# SPDX-FileCopyrightText: Copyright (c) 2022 Connor Feeley
# SPDX-License-Identifier: MIT

{ lib
, buildPythonApplication # mach-nix
, fetchFromGitHub
, ...
}:

buildPythonApplication rec {
  pname = "hdl_checker";
  version = "v0.7.4";

  src = fetchFromGitHub {
    owner = "suoto";
    repo = pname;
    rev = version;
    sha256 = "sha256-iVjKTMR3+hFE5PTwBSk0ZJIfPt7wY+F96hEJ3pZXESc=";
  };

  patches = [
    ./0001-fix-remove-typing-dependency.patch
  ];

  python = "python39";
  # requirementsExtra = "typing-extensions";
  # providers._default = "nixpkgs,wheel,sdist";
  # providers.typing-extensions = "";

  meta = with lib; {
    description = "Language server that wraps VHDL/Verilg/SystemVerilog tools";
    homepage = "https://github.com/suoto/hdl_checker";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ cfeeley ];
  };
}
