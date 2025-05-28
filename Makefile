CC=gcc
CFLAGS=-Wall -Wextra -ggdb -I./include -Wswitch-enum -Wno-char-subscripts -Wno-sign-compare
LDFLAGS=-L./lib
LIBS=

lang: main.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS) $(LIBS)

debug: main.c
	$(CC) $(CFLAGS) -DDEBUG=1 -o lang-debug $< $(LDFLAGS) $(LIBS)

all: lang debug
