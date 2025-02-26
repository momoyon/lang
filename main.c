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

void print_loc(FILE *f, Location loc) {
    /*ASSERT(loc != NULL, "Bro you passed a NULL...");*/
    fprintf(f, "%s:%d:%d", loc.filename, loc.line, loc.col);
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
    String_view lexeme;
    Location loc;
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

void free_lexer(Lexer *l) {
    free(l->src.data);
}

bool eof(Lexer *l) {
    return l->cur >= l->src.count;
}

int col(Lexer *l) {
    return l->cur - l->bol;
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

int ident_predicate(int ch) {
    return !(isalpha(ch) || ch == '_');
}

void consume_ident(Lexer *l, String_view *ident_sv_out, Location *loc_out) {
    // Identifiers can start with [a-z][A-Z]_ and contain [0-9] after the first char
    ASSERT(isalpha(current_char(l)) || current_char(l) == '_', "Called consume_identifier() at the wrong character!");
    // NOTE: Since sv operations modify the sv
    String_view src_copy = {
        .data = l->src.data + l->cur,
        .count = l->src.count - l->cur,
    };

    *ident_sv_out = sv_lpop_until_predicate(&src_copy, ident_predicate);

    loc_out->filename = l->filename;
    loc_out->line     = l->line;
    loc_out->col      = col(l);

    // Advance by the len of ident
    l->cur += ident_sv_out->count;
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

        print_loc(stdout, ident_loc);
        info("Got ident '"SV_FMT"'", SV_ARG(ident_sv));
        t_out->lexeme = ident_sv;
        t_out->loc    = ident_loc;
        return true;
    }

    /*while (isspace(current_char(l))) {*/
    /*    consume_char(l);*/
    /*}*/
    /*ch = current_char(l);*/

    switch (ch) {
        case '"': {
        } break;
        // NOTE: Sanity check
        case ' ': {
            consume_char(l);
        } break;
        default: {
            error("Unhandled char '%c'", ch);
            ASSERT(false, "UNREACHABLE!");
        }
    }

    /*info("ch: '%c'", ch);*/

    return false;
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

    Tokens tokens = lex(&l);

    info("OK");

    free_lexer(&l);

    da_free(tokens);
    return 0;
}
