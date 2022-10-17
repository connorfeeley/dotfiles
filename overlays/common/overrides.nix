channels: final: prev: {
  __dontExport = true;

  ripgrep = prev.ripgrep.override { withPCRE2 = true; };
}
