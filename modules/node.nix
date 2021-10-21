{ pkgs, lib, config, inputs, ... }:

let cfg = config.my.modules.node;
in
{
  options = with lib; {
    my.modules.node = {
      enable = mkEnableOption ''
        Whether to enable node module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      environment.variables = {
        NODE_REPL_HISTORY = "$XDG_DATA_HOME/node/repl_history";
        NVM_DIR = "$XDG_DATA_HOME/node/nvm";
        NVM_AUTO_USE = "true";
        NVM_BIN = "$XDG_BIN_HOME";
        NVM_COMPLETION = "true";
        NVM_LAZY_LOAD = "true";
      };
      my.user.packages = with pkgs; [
        nodejs-16_x
        (yarn.override { nodejs = nodejs-16_x; })
      ];
      my.hm.xdg.configFile = {
        "npm/npmrc" = with config.my; {
          # npmrc requires environment variables to be encosed in `${...}`
          # braces, but Nix will interpret this as antiquotation within its
          # own language. For that reason, we need to escape the `$` character
          # by preceding it with double single-quotes.
          # https://docs.npmjs.com/cli/v7/configuring-npm/npmrc
          # https://nixos.org/manual/nix/stable/#idm140737322046656
          text = ''
            # ${nix_managed}
            # vim:ft=conf
            ${lib.optionalString (email != "") "email=${email}"}
            init-license=MIT
            ${lib.optionalString (email != "") "init-author-email=${email}"}
            ${lib.optionalString (name != "") "init-author-name=${name}"}
            ${lib.optionalString (website != "") "init-author-url=${website}"}
            init-version=0.0.1
            cache=''${XDG_CACHE_HOME}/npm
            tmp=''${XDG_RUNTIME_DIR}/npm
          '';
        };

        "prettier/prettierrc.template" = {
          text = ''
            arrowParens: 'always'
            bracketSpacing: true
            jsxBracketSameLine: false
            jsxSingleQuote: false
            printWidth: 80
            proseWrap: 'never'
            quoteProps: 'as-needed'
            semi: true
            singleQuote: true
            tabWidth: 2
            trailingComma: 'all'
            useTabs: false
            overrides:
              - files: '*.php'
                options:
                  braceStyle: '1tbs'
                  phpVersion: '7.4'
                  tabWidth: 4
                  trailingCommaPHP: true
                  useTabs: true
          '';
        };
      };
    };
}
