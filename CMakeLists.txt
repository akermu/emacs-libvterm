cmake_minimum_required(VERSION 3.0.2)

project(emacs-libvterm C)

# Look for the header file.
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

include_directories(${LIBVTERM_INCLUDE_DIR})

add_library(vterm-module MODULE vterm-module.c utf8.c elisp.c)
set_property(TARGET vterm-module PROPERTY POSITION_INDEPENDENT_CODE ON)
set_target_properties(vterm-module PROPERTIES PREFIX "")
set_target_properties(vterm-module PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

# Link with libvterm
target_link_libraries(vterm-module ${LIBVTERM_LIBRARY})

# Custom run command for testing
add_custom_target(run
  COMMAND emacs -Q -L ${CMAKE_SOURCE_DIR} --eval "\\(require \\'vterm\\)" --eval "\\(vterm\\)"
  DEPENDS vterm-module
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  )
