CFLAGS= -g

all: dfsch

clean:
	rm -f *.o
	rm -f dfsch

dfsch: dfsch.o repl.o
	gcc -o dfsch dfsch.o repl.o -lreadline -lncurses $(CFLAGS)

repl.o: repl.c dfsch.h
	gcc -o repl.o -c repl.c $(CFLAGS)

dfsch.o: dfsch.c dfsch.h
	gcc -o dfsch.o -c dfsch.c $(CFLAGS)


dox:
	doxygen Doxyfile