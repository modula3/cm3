# Needed to compile bootstrap.
add_library(sysutils STATIC)
include(sources.lst)
target_sources(sysutils PRIVATE ${cm3_SOURCES})
