CC     = gcc
CFLAGS = -g -Wall -Wstrict-prototypes -ansi -pedantic

fit: fit.o
	$(CC) fit.o -o fit

fit.o: main.c bci.c bci.h
	$(CC) $(CFLAGS) -c fit.c

check:
	c_style_check fit.c

clean:
	rm -f *.o fit
