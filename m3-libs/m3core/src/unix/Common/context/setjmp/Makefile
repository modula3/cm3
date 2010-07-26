CFLAGS=-I. -g -O3 -fPIC
VPATH=..:../../../../context

tcontext: context.o tcontext.o
	$(CC) $(CFLAGS) context.o tcontext.o -o $@

clean:
	rm -f a.exe tcontext.exe.stackdump context_mips64.o a.exe.stackdump *.o a.out *.o tcontext context.s tcontext tcontext.exe tcontext.core
