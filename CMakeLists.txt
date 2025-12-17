cmake_minimum_required(VERSION 3.18)
project(PyEmbedEmacsModule C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Find Python (for headers + non-macOS link)
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)

# ---------------- Emacs ----------------
find_program(EMACS emacs REQUIRED)
execute_process(
    COMMAND ${EMACS} --version
    OUTPUT_VARIABLE EMACS_VERSION_OUTPUT
    ERROR_QUIET
)
string(REGEX MATCH "([0-9]+)\\.([0-9]+)" EMACS_VERSION_MATCH "${EMACS_VERSION_OUTPUT}")
if(NOT EMACS_VERSION_MATCH)
    message(FATAL_ERROR "Cannot determine Emacs version.")
endif()

set(_emacs_include_paths
    /usr/local/include
    /opt/homebrew/include
    /usr/include
    ${CMAKE_INSTALL_PREFIX}/include
)

find_path(EMACS_INCLUDE_DIR emacs-module.h
    PATHS ${_emacs_include_paths}
    PATH_SUFFIXES emacs-${CMAKE_MATCH_1}.${CMAKE_MATCH_2} emacs
)

if(NOT EMACS_INCLUDE_DIR)
    message(FATAL_ERROR "emacs-module.h not found")
endif()

# ---------------- Module ----------------
# add_library(pyembed MODULE pyembed-module.c)
add_executable(pyembed test.c)

target_include_directories(pyembed PRIVATE
    ${EMACS_INCLUDE_DIR}
    ${Python3_INCLUDE_DIRS}
)

set_target_properties(pyembed PROPERTIES
    PREFIX ""
    OUTPUT_NAME "pyembed"
)

# ---------------- Platform-specific ----------------
if(APPLE)
    target_link_options(pyembed PRIVATE
        -undefined dynamic_lookup
    )

    # Derive actual Python dylib path
    get_target_property(_PYTHON_LIB Python3::Python LOCATION)
    message(STATUS "Python dylib: ${_PYTHON_LIB}")

    if(NOT EXISTS "${_PYTHON_LIB}")
        message(FATAL_ERROR "Python library not found at ${_PYTHON_LIB}")
    endif()

    target_link_options(pyembed PRIVATE
        "-Wl,-weak_library,${_PYTHON_LIB}"
    )
else()
    target_link_libraries(pyembed PRIVATE Python3::Python)
endif()


install(TARGETS pyembed LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/emacs/modules)
