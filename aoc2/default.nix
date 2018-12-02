{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/80738ed9dc0ce48d7796baed5364eef8072c794d.tar.gz";
    sha256 = "0anmvr6b47gbbyl9v2fn86mfkcwgpbd5lf0yf3drgm8pbv57c1dc";
  }) {}
}:
let
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskellPackages.extend (self: super: {
    aoc2 = self.callCabal2nix "aoc2" (lib.sourceByRegex ./. [
      "^src.*$"
      "^input$"
      "^.*\\.cabal$"
    ]) {};
  });
in hpkgs.aoc2 // {
  inherit pkgs hpkgs;
}