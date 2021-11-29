add_executable(mklib)
install(TARGETS mklib)

target_link_libraries(mklib PRIVATE
   Threads::Threads
   m3objfile
)

include(sources.lst)
target_sources(mklib PRIVATE ${cm3_SOURCES})
