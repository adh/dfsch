CFLAGS= -g

all: dfsch s_copy

clean:
	rm -f *.o
	rm -f dfsch s_copy

dfsch: dfsch.o repl.o
	gcc -o dfsch dfsch.o repl.o -lreadline -lncurses $(CFLAGS)

repl.o: repl.c dfsch.h
	gcc -o repl.o -c repl.c $(CFLAGS)

dfsch.o: dfsch.c dfsch.h
	gcc -o dfsch.o -c dfsch.c $(CFLAGS)

stream.o: stream.c stream.h dfsch.h
	gcc -o stream.o -c stream.c $(CFLAGS)

s_copy.o: s_copy.c stream.h dfsch.h
	gcc -o s_copy.o -c s_copy.c $(CFLAGS)

s_copy: s_copy.o stream.o dfsch.o
	gcc -o s_copy s_copy.o stream.o dfsch.o -lreadline -lncurses $(CFLAGS)

dox:
	doxygen Doxyfile