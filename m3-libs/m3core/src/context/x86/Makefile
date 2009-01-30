CFLAGS=-I. -g
VPATH=..

tcontext: context.o contextasm.o tcontext.o
	$(CC) $(CFLAGS) $> -o $@

clean:
	-rm *.o a.out *.o tcontext
