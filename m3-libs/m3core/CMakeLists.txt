# Needed to compile bootstrap.
add_library(m3core STATIC)
target_include_directories(m3core INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_libraries(m3core PUBLIC ${win32_libs})
include(sources.lst)
target_sources(m3core PRIVATE ${cm3_SOURCES})
