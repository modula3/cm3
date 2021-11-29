add_library(m3objfile STATIC)
target_link_libraries(m3objfile PUBLIC m3middle)
include(sources.lst)
target_sources(m3objfile PRIVATE ${cm3_SOURCES})
