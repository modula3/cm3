This isn't really anything original.
It is based on searching the web and trying things out.

I was not able to build much on non-Darwin systems,
due to issues with mbstate_t, building the linker, etc.

The only system I have seen this work on is MacOS X 10.4/PowerPC.
I expect it works on any MacOSX 10.4 or newer system.

I believe it grovels in /Developer for headers.

I believe it produces some incorrect headers, such as sys/stat.h.

This should probably be redone using official Apple source,
at least where it is easily available -- csu, cctools, gcc_42 --
not sure about the headers/libs.
