cmake_minimum_required(VERSION 3.9)
project(foo VERSION 1.0.0 DESCRIPTION "a c++ library that provides the foo function")
include(GNUInstallDirs)
add_library(foo SHARED foo.cpp)
set_target_properties(foo PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION 1
  PUBLIC_HEADER include/foo.hpp)
configure_file(foo.pc.in foo.pc @ONLY)
target_include_directories(foo PRIVATE .)
install(TARGETS foo
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
install(FILES ${CMAKE_BINARY_DIR}/foo.pc
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
