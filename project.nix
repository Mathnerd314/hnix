{ mkDerivation, ansi-wl-pprint, base, containers, data-fix, parsers
, stdenv, tasty, tasty-hunit, tasty-th, text, transformers
, trifecta, unordered-containers, cabal-install, criterion, pkgs
, alex, happy
}:

let
  inherit (builtins) filterSource;
  inherit (pkgs.lib) elem;
in

mkDerivation {
  pname = "hnix";
  version = "0.3.0";
  src = let
    notNamed = list: name: !(elem (baseNameOf name) list);
  in filterSource (n: _: notNamed [".git" "dist" "benchmarks"] n) ./.;
  isLibrary = true;
  isExecutable = true;
  libraryToolDepends = [
    alex happy
  ];
  buildDepends = [
    ansi-wl-pprint base containers data-fix parsers text transformers
    trifecta unordered-containers cabal-install criterion
  ];
  testDepends = [
    base containers data-fix tasty tasty-hunit tasty-th text
  ];
  homepage = "http://github.com/jwiegley/hnix";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
