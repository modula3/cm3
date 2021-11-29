add_library(m3middle STATIC)
target_link_libraries(m3middle PUBLIC m3)
include(sources.lst)
target_sources(m3middle PRIVATE ${cm3_SOURCES})
