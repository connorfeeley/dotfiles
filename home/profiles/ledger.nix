# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, ... }: {
  home.packages = with pkgs; [
    ledger-autosync
    tmuxinator
  ];
  programs.ledger = {
    enable = true;
    package = pkgs.ledger;
    extraConfig = ''
      --sort date
      --effective
      --date-format %Y-%m-%d
    '';
  };
}
