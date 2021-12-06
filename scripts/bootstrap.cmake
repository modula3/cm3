# This file is needed to compile a bootstrap.

# ccache is not strictly necessary for bootstrap or distribution, but
# in saves time in development.
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
   set(CMAKE_C_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
   set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
endif()

# Find system thread libraries.
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

# Find required system libraries for Windows builds.
if(WIN32)
   find_library(COMCTL32 comctl32 REQUIRED)
   find_library(IPHLPAPI iphlpapi REQUIRED)
   find_library(WS2_32   ws2_32   REQUIRED)
   set(win32_libs "${COMCTL32}" "${IPHLPAPI}" "${WS2_32}")
endif()
