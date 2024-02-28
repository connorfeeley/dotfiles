{ pkgs, lib }:

lib.makeScope pkgs.newScope (self: with self; {
  aranet4 = callPackage ./aranet4 { };
  pwrbar = callPackage ./pwrbar { };
})
