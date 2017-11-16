cmake_minimum_required(VERSION 3.0.2)

include(ExternalProject)

project(emacs-libvterm C)

add_library(vterm-module SHARED vterm-module.c utf8.c elisp.c)
set_property(TARGET vterm-module PROPERTY POSITION_INDEPENDENT_CODE ON)
set_target_properties(vterm-module PROPERTIES PREFIX "")
set_target_properties(vterm-module PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

# Link with libvterm
target_link_libraries(vterm-module vterm)

# Link with libutil
target_link_libraries(vterm-module util)

# We need the right emacs-module header
execute_process(COMMAND emacs --batch --eval "(message \"%s.%s\" emacs-major-version emacs-minor-version)"
  ERROR_VARIABLE EMACS_VERSION
  RESULT_VARIABLE RESULT
  ERROR_STRIP_TRAILING_WHITESPACE)
if (NOT ${RESULT} EQUAL 0)
  message(WARNING "Could not detect emacs version. Building module for emacs version 25.3.")
  set(EMACS_VERSION 25.3)
endif()

if (${EMACS_VERSION} VERSION_LESS 25)
  message(FATAL_ERROR "Modules are only supported since emacs version 25.")
endif()

if (${EMACS_VERSION} VERSION_LESS 26)
  ExternalProject_Add(emacs
    URL               "https://ftpmirror.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.gz"
    CONFIGURE_COMMAND ""
    BUILD_COMMAND     ""
    INSTALL_COMMAND   ""
    )
else()
  ExternalProject_Add(emacs
    GIT_REPOSITORY    "git://git.sv.gnu.org/emacs.git"
    CONFIGURE_COMMAND "./autogen.sh" COMMAND "./configure"
    BUILD_COMMAND     ""
    BUILD_IN_SOURCE   "YES"
    INSTALL_COMMAND   ""
    )
endif()
add_dependencies(vterm-module emacs)

ExternalProject_Get_Property(emacs SOURCE_DIR)
include_directories(${SOURCE_DIR}/src)

# Custom run command for testing
add_custom_target(run
    COMMAND emacs -Q -L ${CMAKE_SOURCE_DIR} --eval "\\(require \\'vterm\\)"
    DEPENDS vterm-module
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
