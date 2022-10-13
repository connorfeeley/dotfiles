{ emacsNativeComp
, fetchurl
, fetchFromGitHub
, lib
, darwin
, noTitlebar ? false
, otherIcon ? "default"
}:
let
  emacsPlus = fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    rev = "b7809dd815e7753e20851c81603c82a573d7d1cc";
    sha256 = "sha256-UoMieQKaWB9vSQ75866Kpjb0OKbO1OOj9IwKdAFQit4=";
  };

  iconSources = {
    "emacs-big-sur-icon"                                   = "e9ec41167c38842a3f6555d3142909211a2aa7e3ff91621b9a576b3847d3b565";
    "emacs-icons-project-EmacsIcon1"                       = "50dbaf2f6d67d7050d63d987fe3743156b44556ab42e6d9eee92248c56011bd0";
    "emacs-icons-project-EmacsIcon2"                       = "8d63589b0302a67f13ab94b91683a8ad7c2b9e880eabe008056a246a22592963";
    "emacs-icons-project-EmacsIcon3"                       = "80dd2a4776739a081e0a42008e8444c729d41ba876b19fa9d33fde98ee3e0ebf";
    "emacs-icons-project-EmacsIcon4"                       = "8ce646ca895abe7f45029f8ff8f5eac7ab76713203e246b70dea1b8a21a6c135";
    "emacs-icons-project-EmacsIcon5"                       = "ca415df7ad60b0dc495626b0593d3e975b5f24397ad0f3d802455c3f8a3bd778";
    "emacs-icons-project-EmacsIcon6"                       = "12a1999eb006abac11535b7fe4299ebb3c8e468360faf074eb8f0e5dec1ac6b0";
    "emacs-icons-project-EmacsIcon7"                       = "f5067132ea12b253fb4a3ea924c75352af28793dcf40b3063bea01af9b2bd78c";
    "emacs-icons-project-EmacsIcon8"                       = "d330b15cec1bcdfb8a1e8f8913d8680f5328d59486596fc0a9439b54eba340a0";
    "emacs-icons-project-EmacsIcon9"                       = "f58f46e5ef109fff8adb963a97aea4d1b99ca09265597f07ee95bf9d1ed4472e";
    "emacs-icons-project-emacs-card-blue-deep"             = "6bdb17418d2c620cf4132835cfa18dcc459a7df6ce51c922cece3c7782b3b0f9";
    "emacs-icons-project-emacs-card-british-racing-green"  = "ddf0dff6a958e3b6b74e6371f1a68c2223b21e75200be6b4ac6f0bd94b83e1a5";
    "emacs-icons-project-emacs-card-carmine"               = "4d34f2f1ce397d899c2c302f2ada917badde049c36123579dd6bb99b73ebd7f9";
    "emacs-icons-project-emacs-card-green"                 = "f94ade7686418073f04b73937f34a1108786400527ed109af822d61b303048f7";
    "emacs-sexy-icon"                                      = "7ab72feeeff0084e14bcb75a3e1040bdf738e0044361e7af8a67ebbaa58d852a";
    "gnu-head-icon"                                        = "b5899aaa3589b54c6f31aa081daf29d303047aa07b5ca1d0fd7f9333a829b6d3";
    "modern-icon"                                          = "eb819de2380d3e473329a4a5813fa1b4912ec284146c94f28bd24fbb79f8b2c5";
    "sjrmanning-icon"                                      = "fc267d801432da90de5c0d2254f6de16557193b6c062ccaae30d91b3ada01ab9";
    "spacemacs-icon"                                       = "b3db8b7cfa4bc5bce24bc4dc1ede3b752c7186c7b54c09994eab5ec4eaa48900";
    "retro-sink-bw"                                        = "5cd836f86c8f5e1688d6b59bea4b57c8948026a9640257a7d2ec153ea7200571";
  };

  iconSrc = fetchurl {
    url = "https://github.com/railwaycat/homebrew-emacsmacport/raw/b328061cdba753263c8fabf44be3a54bb1c8c102/icons/${otherIcon}.icns";
    sha256 = iconSources."${otherIcon}";
  };
in
emacsNativeComp.overrideAttrs (o: {
  pname = "emacsPlusNativeComp";

  # https://github.com/cmacrae/emacs/blob/03b4223e56e10a6d88faa151c5804d30b8680cca/flake.nix#L75
  buildInputs = o.buildInputs ++ [ darwin.apple_sdk.frameworks.WebKit ];

  # https://github.com/siraben/nix-gccemacs-darwin/blob/f543cf1d30dc8afb895aaddfb73c92cb739874fe/emacs.nix#L16-L17
  configureFlags =
    o.configureFlags
    ++ [ "--with-cairo" "--with-harfbuzz" ];

  patches =
    lib.optionals noTitlebar [ "${emacsPlus}/patches/emacs-28/no-titlebar.patch" ] ++
    [ "${emacsPlus}/patches/emacs-28/fix-window-role.patch" ];

  # https://github.com/d12frosted/homebrew-emacs-plus#icons
  postPatch = ''
    ${o.postPatch}
  '' +
  (if otherIcon != "default" then
  ''
    cp -f ${iconSrc} nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
  ''
  else "");

  # https://github.com/siraben/nix-gccemacs-darwin/blob/f543cf1d30dc8afb895aaddfb73c92cb739874fe/emacs.nix#L27-L29
  postInstall =
    o.postInstall
    + ''
      ln -snf $out/lib/emacs/${o.version}/native-lisp $out/Applications/Emacs.app/Contents/native-lisp
    '';
})
