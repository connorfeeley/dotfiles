# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, ... }: {
  home.packages = [ pkgs.ledger-autosync ];
  programs.ledger.enable = true;
  programs.ledger.package = pkgs.ledger;
  programs.ledger.extraConfig = ''
    --sort date
    --effective
    --date-format %Y-%m-%d
  '';
}
