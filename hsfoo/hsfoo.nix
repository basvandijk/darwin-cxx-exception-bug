{ mkDerivation, lib, base, bytestring, Cabal, libfoo, inline-c
, inline-c-cpp, stdenv
}:
mkDerivation {
  pname = "hsfoo";
  version = "0.0.0.0";
  src = lib.sourceByRegex ./. [
    "^src$"
    "^src/.*"
    "^test$"
    "^test/.*"
    "^hsfoo.cabal$"
    "^Setup.hs$"
  ];
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [ base bytestring inline-c inline-c-cpp ];
  libraryPkgconfigDepends = [ libfoo ];
  testHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
