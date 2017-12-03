{ stdenv
, mkDerivation
, aeson
, anapo
, async
, base
, bytestring
, containers
, ghcjs-dom
, hashable
, jsaddle
, lens
, safe-exceptions
, text
, time
, transformers
, unliftio-core
, vector
, jsaddle-warp
, ghc
}:

let
  platformHaskellDependencies = if ghc.isGhcjs or false
    then [ ]
    else [ jsaddle-warp ];
  haskellDependencies = [
    aeson
    anapo
    async
    base
    bytestring
    containers
    ghcjs-dom
    hashable
    jsaddle
    lens
    safe-exceptions
    text
    time
    transformers
    vector
  ];
in mkDerivation {
  pname = "anapo-test-app";
  version = "0.1.0.0";
  src = ./.;
  executableHaskellDepends = haskellDependencies ++ platformHaskellDependencies;
  homepage = "https://github.com/bitonic/anapo#readme";
  isLibrary = false;
  isExecutable = true;
  license = stdenv.lib.licenses.bsd3;
}
