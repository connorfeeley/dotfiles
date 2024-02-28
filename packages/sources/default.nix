{ callPackage, pkgs }:
let
  mapPackages = f: with builtins;listToAttrs (map (name: { inherit name; value = f name; }) (filter (v: v != null) (attrValues (mapAttrs (k: v: if v == "directory" && k != "_sources" then k else null) (readDir ./.)))));
  packages = pkgs: mapPackages (name: pkgs.${name});
  overlay = mapPackages (name:
    let
      sources = (import ./_sources/generated.nix) { inherit (pkgs) fetchurl fetchgit fetchFromGitHub dockerTools; };
      package = import ./${name};
      args = builtins.intersectAttrs (builtins.functionArgs package) { source = sources.${name}; };
    in
    callPackage package args
  );
in
overlay
