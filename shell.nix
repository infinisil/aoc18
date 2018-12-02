with import ./default.nix {};

hpkgs.shellFor {
  packages = p: lib.attrValues (lib.filterAttrs (name: value: lib.hasPrefix "aoc" name) p);
  nativeBuildInputs = [ hpkgs.cabal-install ];
}
