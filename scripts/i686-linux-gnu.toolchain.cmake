# Needed to cross-compile i686-linux-gnu target on x86_64-linux-gnu host.

set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR i686)

# Name the cross compilers explicitly: CMake 4.5 (ubuntu-26.04 image
# 20261116) fails in its compiler checks with only *_COMPILER_ARG1.
set(CMAKE_C_COMPILER i686-linux-gnu-gcc)
set(CMAKE_CXX_COMPILER i686-linux-gnu-g++)
set(CMAKE_C_FLAGS_INIT -m32)
set(CMAKE_CXX_FLAGS_INIT -m32)

set(CMAKE_FIND_ROOT_PATH /usr/i686-linux-gnu)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)

include_directories("${CMAKE_FIND_ROOT_PATH}/include")
