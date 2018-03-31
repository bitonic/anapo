{ stdenv
, mkDerivation
, anapo
, base
, vector
, ghcjs-dom
, jsaddle-warp
, random
, lens
, ghc
}:

let
  platformHaskellDependencies = if ghc.isGhcjs or false
    then [ ]
    else [ jsaddle-warp ];
  haskellDependencies = [
    anapo
    base
    vector
    random
    ghcjs-dom
    lens
  ];
in mkDerivation {
  pname = "js-framework-benchmark";
  version = "0.1.0.0";
  src = ./.;
  executableHaskellDepends = haskellDependencies ++ platformHaskellDependencies;
  homepage = "https://github.com/bitonic/anapo#readme";
  isLibrary = false;
  isExecutable = true;
  license = stdenv.lib.licenses.bsd3;
}
