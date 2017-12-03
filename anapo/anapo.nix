{ stdenv
, mkDerivation
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
, ghcjs-base
, aeson
, jsaddle
, text
, ghc
}:

let
  platformHaskellDependencies = if ghc.isGhcjs or false
    then [ ghcjs-base aeson ]
    else [ jsaddle text ];
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
