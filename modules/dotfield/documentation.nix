{ config
, lib
, pkgs
, inputs
, ...
}:
let
  inherit (pkgs.stdenv)
    isLinux
    system
  ;

  inherit (lib)
    types
    mkOption
    mkEnableOption
    ;

    cfg = config.dotfield.documentation;
in
{
  options.dotfield.documentation = {
    enable = mkEnableOption "Whether to enable ALL the documentation, or none of it." // { default = true; };
    program = mkOption {
      description = "Which man page viewer to use.";
      type = types.nullOr (types.enum ([ "mandoc" "man-db" ]));
      # A 5 second search of "mandoc vs. man-db" took me to an r/Linux post about Arch,
      # which concluded that mandoc is absolutely the best program to use EVER and anybody
      # who thinks otherwise "should go back to Windows, fithy fucking casual".
      # My research methodology seemed solid, so mandoc it is.
      default = "mandoc";
    };

  };

  config = {
    ### Cross-platform
    documentation = {
      enable = cfg.enable;
      doc.enable = cfg.enable;
      info.enable = cfg.enable;
      man = {
        enable = cfg.enable;
        generateCaches = cfg.true;
        ${cfg.program}.enable = true;
      };
    };

    ### Linux-only
    documentation = {
      nixos.enable = cfg.enable;
      dev.enable = cfg.enable;
    };

    ### Darwin-only
    documentation = {
      # It's lonely in here.
    };
  };
}
