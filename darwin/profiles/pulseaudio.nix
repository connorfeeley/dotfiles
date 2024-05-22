# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ ... }: {
  homebrew.brews = [
    {
      name = "pulseaudio";
      start_service = true;
      restart_service = "changed";
    }
  ];
}
