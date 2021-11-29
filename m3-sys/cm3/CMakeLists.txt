add_executable(cm3)
install(TARGETS cm3)

target_link_libraries(cm3 PRIVATE
   Threads::Threads
   m3back
   m3front
   m3linker
   m3objfile
   m3quake
   set
   sysutils
)

include(sources.lst)
target_sources(cm3 PRIVATE ${cm3_SOURCES})
