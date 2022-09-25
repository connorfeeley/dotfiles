# SPDX-FileCopyrightText: Copyright (c) 2022 Connor Feeley
# SPDX-License-Identifier: MIT

{ lib
, stdenv
, makeWrapper
, installShellFiles
, ruby_2_7
, sources
, ...
}:

let
  inherit (stdenv.hostPlatform) isDarwin;
in
stdenv.mkDerivation {
  inherit (sources.hlissner-hey) version src;
  pname = "hey";
  nativeBuildInputs = [ makeWrapper installShellFiles ];
  buildInputs = [ ruby_2_7 ];

  prePatch =
    let
      shebangLines = ''
        #!/usr/bin/env cached-nix-shell
        #! nix-shell -p ruby_2_7 -i "ruby -S" --quiet
      '';
    in
    ''
      substituteInPlace bin/hey \
          --replace '${shebangLines}' '#!/usr/bin/env ruby -S'

      substituteInPlace bin/hey \
          --replace 'nixos-rebuild' '${if isDarwin then "darwin" else "nixos"}-rebuild'
    '';

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/bin
    cp -a bin/hey $out/bin/
    wrapProgram "$out/bin/hey" --prefix PATH : "${lib.makeBinPath [ ruby_2_7 ]}"
  '';

  postInstall = ''
    installShellCompletion config/zsh/completions/_hey
  '';
}
