with import ./. {};

hpkgs.shellFor {
  packages = p: [ p.aoc2 ];
  nativeBuildInputs = [ hpkgs.cabal-install ];
}
