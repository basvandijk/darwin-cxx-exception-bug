{ stdenv, cmake } :
stdenv.mkDerivation rec {
  name = "libfoo-${version}";
  version = "1.0.0";
  src = ./.;
  nativeBuildInputs = [ cmake ];

  cmakeFlags = [
    # Be more verbose so we can see the exact compiler and linker commands.
    "-DCMAKE_VERBOSE_MAKEFILE:BOOL=ON"
  ];
}