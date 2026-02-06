cmake_minimum_required(VERSION 3.15)

# Project definition
project(hello-world
    VERSION 1.0.0
    LANGUAGES Fortran
    DESCRIPTION "Hello World Fortran project"
)

# Set Fortran standard
set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Build type default
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# Compiler flags based on build type
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    # GFortran flags
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -Wimplicit-interface -fimplicit-none -cpp")
    set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -fcheck=all -fbacktrace")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native -funroll-loops")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O2 -g -fbacktrace")
    
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    # Intel Fortran flags
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all -fpp")
    set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -check all -traceback")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost -ipo")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O2 -g -traceback")
endif()

# Module output directory
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# Source files
set(SOURCES
    src/strings.f90
    src/main.f90
)

# Executable target
add_executable(${PROJECT_NAME} ${SOURCES})

# Set output directory
set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
)

# Installation rules
install(TARGETS ${PROJECT_NAME}
    RUNTIME DESTINATION bin
)

# Print build information
message(STATUS "")
message(STATUS "=== Build Configuration ===")
message(STATUS "Project: ${PROJECT_NAME} v${PROJECT_VERSION}")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
message(STATUS "Fortran flags: ${CMAKE_Fortran_FLAGS}")
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    message(STATUS "Debug flags: ${CMAKE_Fortran_FLAGS_DEBUG}")
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
    message(STATUS "Release flags: ${CMAKE_Fortran_FLAGS_RELEASE}")
endif()
message(STATUS "Install prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "===========================")
message(STATUS "")
