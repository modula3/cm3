add_library(set STATIC)
include(sources.lst)
target_sources(set PRIVATE ${cm3_SOURCES})
