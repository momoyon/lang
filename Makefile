CC=gcc
CFLAGS=-Wall -Wextra -ggdb -I./include -Wswitch-enum
LDFLAGS=-L./lib
LIBS=

lang: main.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS) $(LIBS)
