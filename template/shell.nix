with import ./. {};

hpkgs.shellFor {
  packages = p: [ p.aoc@day@ ];
  nativeBuildInputs = [ hpkgs.cabal-install ];
}
