add_library(m3linker STATIC)
target_link_libraries(m3linker PUBLIC m3)
include(sources.lst)
target_sources(m3linker PRIVATE ${cm3_SOURCES})
