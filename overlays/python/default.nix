{ inputs }:
channels: final: prev:
let
  packagesFrom = inputAttr: inputAttr.packages.${final.system};
in
rec {
  inherit (packagesFrom inputs.chatgpt-wrapper) chatgpt-wrapper;

  pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
    (python-final: python-prev: {
      chatgpt-wrapper = chatgpt-wrapper;
    })
  ];
}
