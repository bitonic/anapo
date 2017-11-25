{ ghcjs ? true
, stdenv
, mkDerivation
# platform independent deps
, async
, base
, dlist
, ghcjs-dom
, hashable
, lens
, safe-exceptions
, time
, transformers
, unliftio-core
, unordered-containers
# all the packages for the  platform-dependent ones
, haskellPackages
}:

let
  platformHaskellDependencies = if ghcjs
    then [ haskellPackages.ghcjs-base ]
    else [ haskellPackages.jsaddle haskellPackages.text ];
  haskellDependencies = [
    async
    base
    dlist
    ghcjs-dom
    hashable
    lens
    safe-exceptions
    time
    transformers
    unliftio-core
    unordered-containers
  ];
in mkDerivation {
  pname = "anapo";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = haskellDependencies ++ platformHaskellDependencies;
  homepage = "https://github.com/bitonic/anapo#readme";
  license = stdenv.lib.licenses.bsd3;
}
