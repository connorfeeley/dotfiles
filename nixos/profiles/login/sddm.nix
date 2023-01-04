{ config
, ...
}: {
  services.xserver = {
    displayManager = {
      sddm.enable = true;
      sddm.enableHidpi = true;
      # sddm.settings = { Autologin = { Session = "plasma.desktop"; User = "john"; } ; };
    };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [ hp.xmonad hp.xmonad-contrib hp.xmonad-extras ];

      config = ''
        import XMonad
        import XMonad.Util.EZConfig (additionalKeys)
        import Control.Monad (when)
        import Text.Printf (printf)
        import System.Posix.Process (executeFile)
        import System.Info (arch,os)
        import System.Environment (getArgs)
        import System.FilePath ((</>))
        compiledConfig = printf "xmonad-%s-%s" arch os
        myConfig = defaultConfig
          { modMask = mod4Mask -- Use Super instead of Alt
          , terminal = "urxvt" }
          `additionalKeys`
          [ ( (mod4Mask,xK_r), compileRestart True)
          , ( (mod4Mask,xK_q), restart "xmonad" True ) ]
        compileRestart resume = do
          dirs  <- asks directories
          whenX (recompile dirs True) $ do
            when resume writeStateToFile
            catchIO
                ( do
                    args <- getArgs
                    executeFile (cacheDir dirs </> compiledConfig) False args Nothing
                )
        main = getDirectories >>= launch myConfig
      '';
    };
  };
}
