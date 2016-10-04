# Make sure to use a C99 compatible C compiler.
# Change CC to your compiler, and CFLAGS, if needed.
CC=gcc
CFLAGS=-Wall -std=c99
LDFLAGS=
OBJ=picoi.o

picoi: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^

%.o : %.c
	$(CC) -c $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	rm *.o picoi
