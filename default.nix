{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/80738ed9dc0ce48d7796baed5364eef8072c794d.tar.gz";
    sha256 = "0anmvr6b47gbbyl9v2fn86mfkcwgpbd5lf0yf3drgm8pbv57c1dc";
  }) {}
}:
let
  inherit (pkgs) lib;
  hpkgs = pkgs.haskellPackages.extend extension;

  days = lib.filter (lib.hasPrefix "aoc") (lib.attrNames (builtins.readDir ./.));

  extension = self: super: lib.genAttrs days (name:
    self.callCabal2nix name (lib.sourceByRegex (./. + "/${name}") [
      "^src.*$"
      "^input$"
      "^.*\\.cabal$"
    ]) {}
  );

  packages = lib.genAttrs days (name: hpkgs.${name});

  combinedBinary = pkgs.writeScriptBin "aoc" (''
    #!${pkgs.stdenv.shell}
  '' + lib.concatMapStrings (day: ''
    ${packages.${day}}/bin/${day}
  '') days);

  combined = pkgs.symlinkJoin {
    name = "aoc";
    paths = [combinedBinary] ++ lib.attrValues packages;
  };

in combined // packages // {
  inherit pkgs lib hpkgs;
}
