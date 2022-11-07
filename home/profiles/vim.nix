{ self
, config
, lib
, pkgs
, ...
}:
let
  inherit (self.inputs) pta2002-neovim;
in {
  imports = [ pta2002-neovim.homeManagerModules.nixvim ];

  config = {
    home.packages = [ ];

    # pta2022's neovim flake
    programs.nixvim.enable = true;
  };
}
