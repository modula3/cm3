# Needed to compile bootstrap.
add_library(m3back STATIC)
target_link_libraries(m3back PUBLIC m3)
include(sources.lst)
target_sources(m3back PRIVATE ${cm3_SOURCES})
