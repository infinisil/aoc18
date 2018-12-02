with import ./. {};

hpkgs.shellFor {
  packages = p: [ p.aoc1 ];
  nativeBuildInputs = [ hpkgs.cabal-install ];
}
