# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ lib, nixos-rebuild, nix-output-monitor }:

# Override nixos-rebuild to use nix-output-monitor
nixos-rebuild.overrideAttrs (oldAttrs: {
  name = "nomos-rebuild";

  src = oldAttrs.src;

  path = lib.makeBinPath [ oldAttrs.path nix-output-monitor ];

  postInstall = ''
    substituteInPlace $out/bin/nomos-rebuild --replace-fail 'runCmd nix-build' 'runCmd nom-build'
  '';

  meta.mainProgram = "nomos-rebuild";
})
