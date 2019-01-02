{ stdenv, cmake } :
stdenv.mkDerivation rec {
  name = "libfoo-${version}";
  version = "1.0.0";
  src = ./.;
  nativeBuildInputs = [ cmake ];
}