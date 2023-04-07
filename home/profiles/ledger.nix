# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, ... }: {
  home.packages = with pkgs; [
    ledger-autosync
    tmuxinator
    hledger-ui
    hledger-utils
    hledger-web
    hledger-interest
    # hledger-iadd
    hledger-check-fancyassertions
  ];
  programs.ledger = {
    enable = true;
    package = pkgs.hledger;
    extraConfig = ''
      --sort date
      --effective
      --date-format %Y-%m-%d
    '';
  };
}
