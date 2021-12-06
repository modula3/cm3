# Needed to compile bootstrap.
add_library(m3 STATIC)
target_link_libraries(m3 PUBLIC m3core)
include(sources.lst)
target_sources(m3 PRIVATE ${cm3_SOURCES})
