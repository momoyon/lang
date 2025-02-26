#include <stdio.h>
#include <ctype.h>

#define COMMONLIB_IMPLEMENTATION
#define COMMONLIB_REMOVE_PREFIX
#include <commonlib.h>

#define error log_error
#define info log_info

void usage(const char *program) {
    info("Usage: %s <file>", program);
}

typedef struct {
    const char *filename;
    int line;
    int col;
} Location;

void print_loc(FILE *f, Location *loc) {
    ASSERT(loc != NULL, "Bro you passed a NULL...");
    fprintf(f, "%s:%d:%d", loc->filename, loc->line, loc->col);
}

typedef struct {
    // NOTE: src gets data from a heap allocated string!!!
    String_view src;
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
        .src = sv_from_cstr(buf),
        .cur = 0,
        .bol = 0,
        .line = 1,
        .filename = filename,
    };

    return l;
}

bool eof(Lexer *l) {
    return l->cur >= l->src.count;
}

char current_char(Lexer *l) {
    ASSERT(!eof(l), "Trying to get char after EOF");
    return l->src.data[l->cur];
}

char consume_char(Lexer *l) {
    char ch = current_char(l);
    l->cur += 1;
    return ch;
}

void consume_ident(Lexer *l, String_view *ident_sv_out, Location *loc_out) {
    // Identifiers can start with [a-z][A-Z]_ and contain [0-9] after the first char
    ASSERT(isalpha(current_char(l)) || current_char(l) == '_', "Called consume_identifier() at the wrong character!");
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
    (void)t_out;
    left_trim(l);

    if (eof(l)) return false;

    char ch = current_char(l);

    if (isalpha(ch) || ch == '_') {
        String_view ident_sv = {0};
        Location ident_loc = {0};
        consume_ident(l, &ident_sv, &ident_loc);
    }

    switch (ch) {
        case '"': {
        } break;
        default: {
            error("Unhandled char '%c'", ch);
            ASSERT(false, "UNREACHABLE!");
        }
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
