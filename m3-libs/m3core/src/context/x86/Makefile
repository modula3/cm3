CFLAGS=-I. -g
VPATH=..

tcontext: context.o contextasm.o tcontext.o
	$(CC) $(CFLAGS) context.o contextasm.o tcontext.o -o $@

clean:
	-rm *.o a.out *.o tcontext context.s tcontext tcontext.exe 2> /dev/null
