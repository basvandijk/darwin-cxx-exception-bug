{ stdenv, cmake, pkgconfig, libfoo } :
stdenv.mkDerivation rec {
  name = "foo-${version}";
  version = "1.0.0";
  src = ./.;
  nativeBuildInputs = [ cmake pkgconfig ];
  buildInputs = [ libfoo ];

  cmakeFlags = [
    # Be more verbose so we can see the exact compiler and linker commands.
    "-DCMAKE_VERBOSE_MAKEFILE:BOOL=ON"
  ];
}