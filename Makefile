CC=gcc
CFLAGS=-Wall -Wextra -ggdb -I./include
LDFLAGS=-L./lib
LIBS=

lang: main.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS) $(LIBS)
