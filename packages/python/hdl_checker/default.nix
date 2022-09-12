# SPDX-FileCopyrightText: Copyright (c) 2022 Connor Feeley
# SPDX-License-Identifier: MIT

{ lib
, buildPythonApplication # mach-nix
, fetchurl
, ...
}:

buildPythonApplication {
  src = builtins.fetchGit {
    url = "https://github.com/suoto/hdl_checker.git";
    ref = "master";
    rev = "12983254962ca2d221d5e755726528aedeca27e2";
  };
  python = "python310";
  meta = with lib; {
    description = "Language server that wraps VHDL/Verilg/SystemVerilog tools";
    homepage = "https://github.com/suoto/hdl_checker";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ cfeeley ];
  };
}
