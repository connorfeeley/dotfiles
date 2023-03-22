# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, ... }: {
  home.packages = with pkgs; [
      ## === Media Tools ===
      ffmpeg-full # <- Full ffmpeg with all codecs
      chafa # <- "terminal graphics for the 21st century"
      yt-dlp # <- youtube-dl fork
      libgen-cli # <- query Library Genesis database
      mpv
  ];
}
