# Needed to compile bootstrap.
add_library(m3quake STATIC)
target_link_libraries(m3quake PUBLIC m3)
include(sources.lst)
target_sources(m3quake PRIVATE ${cm3_SOURCES})
