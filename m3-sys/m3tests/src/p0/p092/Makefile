M3 = /proj/packages/m3exp/latest/bin/m3
# M3 = m3
OBJECTS = Main.mo foo.mo foo.io

all: test

test: $(OBJECTS) ; $(M3) -o test $(OBJECTS)

.SUFFIXES: .m3 .i3 .mo .io
.m3.mo: ; $(M3) -c -k -X0-C $*.m3
.i3.io: ; $(M3) -c $*.i3
