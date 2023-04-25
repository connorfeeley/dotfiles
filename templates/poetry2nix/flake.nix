# ##  SETUP

{
  description = "Application packaged using poetry2nix";

  inputs = {
    flake-utils = { url = "github:numtide/flake-utils"; };
    devshell = {
      url = "github:numtide/devshell";
      inputs.flake-utils.follows = "flake-utils";
    };
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, devshell, poetry2nix }:
    let
      cryptographyOverrides = (final: prev:
        prev.poetry2nix.overrides.withDefaults (self: super: {
          foo = null;
          cryptography = super.cryptography.overridePythonAttrs (old: {
            cargoDeps = super.pkgs.rustPlatform.fetchCargoTarball {
              inherit (old) src;
              name = "${old.pname}-${old.version}";
              sourceRoot = "${old.pname}-${old.version}/src/rust/";
              sha256 = "sha256-yrPpwbwopt8a8+d7Or1Na3kuhncTQPKlFnjGWYfHSK0=";
            };
            cargoRoot = "src/rust";
            nativeBuildInputs = old.nativeBuildInputs
              ++ (with super.pkgs.rustPlatform; [
              rust.rustc
              rust.cargo
              cargoSetupHook
            ]);
          });
        }));
    in
    {
      # Nixpkgs overlay providing the application
      overlays.default = nixpkgs.lib.composeManyExtensions [
        poetry2nix.overlay
        (final: prev: {
          # The application
          poetry2nixTemplateEnv = prev.poetry2nix.mkPoetryEnv {
            python = prev.pkgs.python311;
            projectDir = ./.;
            extraPackages = ps: with ps; [ ipython ];
            overrides = prev.poetry2nix.defaultPoetryOverrides.extend
              (self: super: {
                pathspec = super.pathspec.overridePythonAttrs (old: {
                  buildInputs = (old.buildInputs or [ ]) ++ [ super.flit-core ];
                });
              });
          };
        })
      ];
    } // (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default devshell.overlay ];
        };
      in
      {
        devShells.default = pkgs.devshell.mkShell {
          name = "poetry2nixTemplate";

          commands = with pkgs; [
            {
              category = "python";
              package = poetry2nix.packages.${system}.poetry;
            }
            {
              category = "python";
              package = poetry2nix.packages.${system}.poetry2nix;
            }
            {
              category = "python";
              package = nodePackages.pyright;
            }
          ];

          packages = with pkgs; [ templateEnv ];
        };
      }));
}
