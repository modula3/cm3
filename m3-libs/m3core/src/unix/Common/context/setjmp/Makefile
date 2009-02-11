CFLAGS=-I. -g
VPATH=..:../../../../context

tcontext: context.o contextasm.o tcontext.o
	$(CC) $(CFLAGS) context.o contextasm.o tcontext.o -o $@

clean:
	-rm a.exe tcontext.exe.stackdump a.exe.stackdump *.o a.out *.o tcontext context.s tcontext tcontext.exe 2> /dev/null
