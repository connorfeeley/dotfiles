# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ ... }: {
  homebrew.brews = [{
    name = "pulseaudio";
    start_service = true;
    restart_service = "changed";
    link = true;
  }];

  # Setup instructions:
  # mkdir /opt/homebrew/Cellar/pulseaudio/17.0/etc/pulse/default.pa.d
  # ln -s /opt/homebrew/Cellar/pulseaudio/17.0/etc/pulse/default.pa.d /opt/homebrew/etc/pulse/default.pa.d
  # echo 'load-module module-native-protocol-tcp auth-anonymous=1 auth-ip-acl=127.0.0.1;100.84.241.13;192.168.0.0/24' > /opt/homebrew/etc/pulse/default.pa.d/99-pulseaudio-tcp.conf
}
