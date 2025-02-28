CC=gcc
CFLAGS=-Wall -Wextra -ggdb -I./include -fsanitize=address -Wswitch-enum
LDFLAGS=-L./lib
LIBS=

lang: main.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS) $(LIBS)
