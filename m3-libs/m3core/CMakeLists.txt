# Needed to compile bootstrap.
add_library(m3core STATIC)
target_include_directories(m3core INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_libraries(m3core PUBLIC ${win32_libs})
# only add quadmath to link for AMD64_LINUX target
if(M3Target STREQUAL "AMD64_LINUX")
target_link_libraries(m3core quadmath)
endif()
include(sources.lst)
target_sources(m3core PRIVATE ${cm3_SOURCES})
