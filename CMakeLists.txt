cmake_minimum_required(VERSION 3.11)
include(ExternalProject)

project(emacs-libvterm C)

if(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
   set(LIBVTERM_BUILD_COMMAND "gmake")
else()
   set(LIBVTERM_BUILD_COMMAND "make")
endif()

add_library(vterm-module MODULE vterm-module.c utf8.c elisp.c)
set_target_properties(vterm-module PROPERTIES
  C_STANDARD 99
  POSITION_INDEPENDENT_CODE ON
  PREFIX ""
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}
  )

# Look for the header file.
option(USE_SYSTEM_LIBVTERM "Use system libvterm instead of the vendored version." OFF)

if(USE_SYSTEM_LIBVTERM)
  find_path(LIBVTERM_INCLUDE_DIR NAMES
    vterm.h
    )

  if(NOT LIBVTERM_INCLUDE_DIR)
    message(FATAL_ERROR "vterm.h not found")
  endif()

  find_library(LIBVTERM_LIBRARY NAMES
    vterm
    libvterm
    )

  if(NOT LIBVTERM_LIBRARY)
    message(FATAL_ERROR "libvterm not found")
  endif()
else()
  ExternalProject_add(libvterm
    GIT_REPOSITORY https://github.com/neovim/libvterm.git
    GIT_TAG 89675ffdda615ffc3f29d1c47a933f4f44183364
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ${LIBVTERM_BUILD_COMMAND} "CFLAGS='-fPIC'"
    BUILD_IN_SOURCE ON
    INSTALL_COMMAND "")

  ExternalProject_Get_property(libvterm SOURCE_DIR)

  set(LIBVTERM_INCLUDE_DIR ${SOURCE_DIR}/include)
  set(LIBVTERM_LIBRARY ${SOURCE_DIR}/.libs/libvterm.a)

  add_dependencies(vterm-module libvterm)

  # Workaround for https://gitlab.kitware.com/cmake/cmake/issues/15052
  file(MAKE_DIRECTORY ${LIBVTERM_INCLUDE_DIR})
endif()

add_library(vterm STATIC IMPORTED)
set_target_properties(vterm PROPERTIES IMPORTED_LOCATION ${LIBVTERM_LIBRARY})
target_include_directories(vterm INTERFACE ${LIBVTERM_INCLUDE_DIR})

# Link with libvterm
target_link_libraries(vterm-module PUBLIC vterm)

# Custom run command for testing
add_custom_target(run
  COMMAND emacs -Q -L ${CMAKE_SOURCE_DIR} -L ${CMAKE_BINARY_DIR} --eval "\\(require \\'vterm\\)" --eval "\\(vterm\\)"
  DEPENDS vterm-module
  )
