add_library(m3front STATIC)
target_link_libraries(m3front PUBLIC m3)
include(sources.lst)
target_sources(m3front PRIVATE ${cm3_SOURCES})
