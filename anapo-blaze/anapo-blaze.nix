{ stdenv
, mkDerivation
, anapo
, text
, blaze-builder
}:

let
  haskellDependencies = [
    anapo
    text
    blaze-builder
  ];
in mkDerivation {
  pname = "anapo-blaze";
  version = "0.1.0.0";
  src = ./.;
  executableHaskellDepends = haskellDependencies;
  homepage = "https://github.com/bitonic/anapo#readme";
  isLibrary = true;
  isExecutable = false;
  license = stdenv.lib.licenses.bsd3;
}
