#include <stdio.h>

#define COMMONLIB_IMPLEMENTATION
#define COMMONLIB_REMOVE_PREFIX
#include <commonlib.h>

#define error log_error
#define info log_info

void usage(const char *program) {
    info("Usage: %s <file>", program);
}

typedef struct {
    const char *src;
    size_t      src_len;
    size_t cur;
    size_t bol; // Beginning of Line
    size_t line;
    const char *filename;
} Lexer;

typedef struct {
    const char *lexeme;
} Token;

typedef struct {
    Token *items;
    size_t count;
    size_t capacity;
} Tokens;

Lexer make_lexer(const char *filename) {
    bool ok = false;
    const char *buf = slurp_file(filename, &ok);
    if (!ok) {
        error("Failed to open '%s'", filename);
        exit(1);
    }
    Lexer l = {
        .src = buf,
        .src_len = strlen(buf),
        .cur = 0,
        .bol = 0,
        .line = 1,
        .filename = filename,
    };

    return l;
}

bool eof(Lexer *l) {
    return l->cur >= l->src_len;
}

char current_char(Lexer *l) {
    ASSERT(!eof(l), "Trying to get char after EOF");
    return l->src[l->cur];
}

char consume_char(Lexer *l) {
    char ch = current_char(l);
    l->cur += 1;
    return ch;
}

void left_trim(Lexer *l) {
    while (!eof(l) && isspace(current_char(l))) {
        // TODO: Care about window's \r\n....
        if (current_char(l) == '\n') {
            l->line += 1;
            l->bol = l->cur + 1;
        }
        consume_char(l);
    }
}

bool next_token(Lexer *l, Token *t_out) {
    left_trim(l);

    if (eof(l)) return false;

    char ch = current_char(l);

    switch (ch) {
    }

    /*info("ch: '%c'", ch);*/

    return true;
}

Tokens lex(Lexer *l) {
    Tokens tokens = {0};
    Token t = {0};
    while (next_token(l, &t)) {
        da_append(tokens, t);
    }

    return tokens;
}

int main(int argc, char **argv) {
    const char *program = shift_args(argv, argc);

    if (argc <= 0) {
        error("Please provide a filename!");
        usage(program);
        return 1;
    }

    const char *filename = shift_args(argv, argc);

    Lexer l = make_lexer(filename);

    lex(&l);

    info("OK");
    return 0;
}
