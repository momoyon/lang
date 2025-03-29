#include <stdio.h>
#include <ctype.h>

#define COMMONLIB_IMPLEMENTATION
#define COMMONLIB_REMOVE_PREFIX
#include <commonlib.h>

#define error log_error
#define info log_info

#define COMPILER_VERSION "v0.0.3"

static bool DEBUG_PRINT = false;

void usage(const char *program) {
    info("Usage: %s [flag(s)] <file>", program);
}

void help(const char *program) {
    usage(program);

    info("");
    info("Flags: ");
    info("  -h      Prints this help message.");
    info("  -v      Prints the version of the compiler.");
    info("  -d      Enables debug printing.");
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

#define compiler_error(loc, fmt, ...) do { \
        print_loc(stderr, loc);\
        putc(' ', stderr);\
        error(fmt, ##__VA_ARGS__);\
    } while (0)

typedef enum {
    TK_IDENT,
    TK_KEYWORD,

    TK_COMMENT,
    TK_MULTILINE_COMMENT,

    TK_STRING,

    TK_LEFT_PAREN,
    TK_RIGHT_PAREN,
    TK_MINUS,
    TK_MINUS_MINUS,
    TK_MINUS_EQUAL,
    TK_PLUS,
    TK_PLUS_PLUS,
    TK_PLUS_EQUAL,
    TK_RETURNER,
    TK_LEFT_BRACE,
    TK_RIGHT_BRACE,
    TK_DIVIDE,
    TK_DIVIDE_EQUAL,
    TK_MULTIPLY,
    TK_MULTIPLY_EQUAL,
    TK_MODULUS,
    TK_MODULUS_EQUAL,
    TK_POWER,
    TK_EQUAL,
    // These three are logical
    TK_NOT,
    TK_NOT_EQUAL,
    TK_EQUAL_EQUAL,
    TK_GT,
    TK_LT,
    TK_GTE,
    TK_LTE,
    TK_COMMA,
    TK_COLON,
    TK_SEMICOLON,
    TK_DOT,
    TK_HASH,
    TK_LEFT_SQUARE_BRACE,
    TK_RIGHT_SQUARE_BRACE,

    TK_INT,
    TK_FLOAT,

    TK_BITWISE_AND,
    TK_BITWISE_AND_EQUAL,
    TK_BITWISE_NOT,
    TK_BITWISE_OR,
    TK_BITWISE_OR_EQUAL,
    TK_BITWISE_XOR,
    TK_BITWISE_XOR_EQUAL,
    TK_BITWISE_SHIFT_LEFT,
    TK_BITWISE_SHIFT_RIGHT,
    TK_LOGICAL_AND,
    TK_LOGICAL_OR,

    TK_COUNT,
} Token_type;

typedef struct {
    String_view lexeme;
    Location loc;
    Token_type type;
} Token;

typedef struct {
    Token *items;
    size_t count;
    size_t capacity;
} Tokens;

typedef struct Expression Expression;
typedef struct Binary_expression Binary_expression;
typedef struct Grouping Grouping;
typedef struct Literal Literal;

struct Grouping {
    Expression *expr;
};

struct Literal {
    union {
        float f;
        int i;
        bool b;
        char ch;
        char *str;
    } as;
};

struct Binary_expression {
    Expression *lhs;
    Token      *op;
    Expression *rhs;
};

struct Expression {
    Binary_expression bin_expr;
};

typedef struct {
    // NOTE: src gets data from a heap allocated string!!!
    String_view src;
    size_t cur;
    size_t bol; // Beginning of Line
    size_t line;
    const char *filename;
} Lexer;

typedef struct {
    Tokens tokens;
} Parser;

const char *token_type_as_str(Token_type t) {
    switch (t) {
        case TK_IDENT: return "IDENT";
        case TK_KEYWORD: return "KEYWORD";
        case TK_COMMENT: return "COMMENT";
        case TK_MULTILINE_COMMENT: return "MULTILINE_COMMENT";
        case TK_STRING: return "STRING";
        case TK_LEFT_PAREN: return "LEFT_PAREN";
        case TK_RIGHT_PAREN: return "RIGHT_PAREN";
        case TK_MINUS: return "MINUS";
        case TK_MINUS_MINUS: return "MINUS_MINUS";
        case TK_MINUS_EQUAL: return "MINUS_EQUAL";
        case TK_PLUS: return "PLUS";
        case TK_PLUS_PLUS: return "PLUS_PLUS";
        case TK_PLUS_EQUAL: return "PLUS_EQUAL";
        case TK_RETURNER: return "RETURNER";
        case TK_LEFT_BRACE: return "LEFT_BRACE";
        case TK_RIGHT_BRACE: return "RIGHT_BRACE";
        case TK_DIVIDE: return "DIVIDE";
        case TK_DIVIDE_EQUAL: return "DIVIDE_EQUAL";
        case TK_MULTIPLY: return "MULTIPLY";
        case TK_MULTIPLY_EQUAL: return "MULTIPLY_EQUAL";
        case TK_MODULUS: return "MODULUS";
        case TK_MODULUS_EQUAL: return "MODULUS_EQUAL";
        case TK_POWER: return "POWER";
        case TK_EQUAL: return "EQUAL";
        case TK_NOT: return "NOT";
        case TK_NOT_EQUAL: return "NOT_EQUAL";
        case TK_EQUAL_EQUAL: return "EQUAL_EQUAL";
        case TK_GT: return "GT";
        case TK_LT: return "LT";
        case TK_GTE: return "GTE";
        case TK_LTE: return "LTE";
        case TK_COMMA: return "COMMA";
        case TK_COLON: return "COLON";
        case TK_SEMICOLON: return "SEMICOLON";
        case TK_DOT: return "DOT";
        case TK_HASH: return "HASH";
        case TK_LEFT_SQUARE_BRACE: return "LEFT_SQUARE_BRACE";
        case TK_RIGHT_SQUARE_BRACE: return "RIGHT_SQUARE_BRACE";
        case TK_INT: return "INT";
        case TK_FLOAT: return "FLOAT";
        case TK_BITWISE_AND: return "BITWISE_AND";
        case TK_BITWISE_AND_EQUAL: return "BITWISE_AND_EQUAL";
        case TK_BITWISE_NOT: return "BITWISE_NOT";
        case TK_BITWISE_OR: return "BITWISE_OR";
        case TK_BITWISE_OR_EQUAL: return "BITWISE_OR_EQUAL";
        case TK_BITWISE_XOR: return "BITWISE_XOR";
        case TK_BITWISE_XOR_EQUAL: return "BITWISE_XOR_EQUAL";
        case TK_BITWISE_SHIFT_LEFT: return "BITWISE_SHIFT_LEFT";
        case TK_BITWISE_SHIFT_RIGHT: return "BITWISE_SHIFT_RIGHT";
        case TK_LOGICAL_AND: return "LOGICAL_AND";
        case TK_LOGICAL_OR: return "LOGICAL_OR";
        case TK_COUNT:
        default: {
            ASSERT(false, "UNREACHABLE");
        }
    }
}

const char *keywords[] = {
    "int",
    "int8",
    "int16",
    "int32",
    "int64",

    "uint",
    "uint8",
    "uint16",
    "uint32",
    "uint64",

    "float",
    "float32",
    "float64",
    "char",
    "string",
    "bool",

    "if",
    "else",

    "for",
    "while",

    "fun",

    "enum",
    "struct",
    "union",

    // Yes include will be part of the language, not part of a preprocessor
    "include",

    "return",
    "continue",
    "switch",
    "break",
    "case",
    "default",
};

bool is_keyword(String_view ident) {
    for (size_t i = 0; i < ARRAY_LEN(keywords); ++i) {
        if (sv_equals(ident, SV(keywords[i]))) return true;
    }
    return false;
}

// TODO: Should be differentiate hex, octal and binary here too?
Token_type number_token_type(String_view number) {
    if (sv_contains_char(number, '.')) {
        return TK_FLOAT;
    }

    return TK_INT;
}

// NOTE: We assume only multiline comments contain newlines
Token_type comment_token_type(String_view comment) {
    if (sv_contains_char(comment, '\n')) return TK_MULTILINE_COMMENT;
    return TK_COMMENT;
}

void print_token(FILE *f, Token t) {
    print_loc(f, t.loc);
    fprintf(f, " [%s] '"SV_FMT"'", token_type_as_str(t.type), SV_ARG(t.lexeme));
}

Lexer make_lexer(const char *filename) {
    bool ok = false;
    const char *buf = slurp_file(filename, &ok);
    if (!ok) {
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

Parser make_parser(Tokens tokens) {
    return (Parser) {
        .tokens = tokens,
    };
}

void free_parser(Parser *p) {
    da_free(p->tokens);
}

String_view get_src_copy(Lexer *l) {
    String_view src_copy = {
        .data = l->src.data + l->cur,
        .count = l->src.count - l->cur,
    };
    return src_copy;
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

// NOTE: returns 0 if next char is after EOF
char next_char(Lexer *l) {
    if (l->cur+1 >= l->src.count) return 0;
    return l->src.data[l->cur+1];
}

char consume_char(Lexer *l) {
    char ch = current_char(l);
    l->cur += 1;
    return ch;
}

int not_ident_predicate(int ch) {
    return !(isalpha(ch) || ch == '_');
}

int not_number_predicate(int ch) {
    return !isdigit(ch);
}

int not_number_or_ident_predicate(int ch) {
    return !(isalpha(ch) || ch == '_' || isdigit(ch));
}

void consume_ident(Lexer *l, String_view *ident_sv_out, Location *loc_out) {
    // Identifiers can start with [a-z][A-Z]_ and contain [0-9] after the first char
    ASSERT(isalpha(current_char(l)) || current_char(l) == '_', "Called consume_identifier() at the wrong character!");
    // NOTE: Since sv operations modify the sv
    String_view src_copy = get_src_copy(l);
    String_view first_char = sv_lpop(&src_copy, 1);
    ident_sv_out->data = first_char.data;
    ident_sv_out->count = first_char.count;
    if (first_char.count && !not_ident_predicate(first_char.data[0])) {
        String_view rest_of_the_ident = sv_lpop_until_predicate(&src_copy, not_number_or_ident_predicate);
        ident_sv_out->count += rest_of_the_ident.count;
    }


    loc_out->filename = l->filename;
    loc_out->line     = l->line;
    loc_out->col      = col(l);

    // Advance by the len of ident
    l->cur += ident_sv_out->count;
}

void consume_string(Lexer *l, String_view *string_sv_out, Location *loc_out) {
    ASSERT(current_char(l) == '"', "We except '\"' to be the current_char here...");

    // Eat "
    consume_char(l);
    String_view src_copy = get_src_copy(l);

    *string_sv_out = sv_lpop_until_char(&src_copy, '"');

    loc_out->filename = l->filename;
    loc_out->line     = l->line;
    loc_out->col      = col(l);

    // Advance by the len of sv
    l->cur += string_sv_out->count;

    if (eof(l)) {
        compiler_error(*loc_out, "Unterminated string!"); 
        exit(1);
    }

    // Eat "
    consume_char(l);
}

void consume_single_char(Lexer *l, String_view *sv_out, Location *loc_out) {

    String_view src_copy = get_src_copy(l);

    *sv_out = sv_lpop(&src_copy, 1);

    loc_out->filename = l->filename;
    loc_out->line     = l->line;
    loc_out->col      = col(l);

    // Advance by the len of sv
    l->cur += sv_out->count;
}

void consume_number(Lexer *l, String_view *sv_out, Location *loc_out) {
    ASSERT(isdigit(current_char(l)), "We expect a number bro...");

    String_view src_copy = get_src_copy(l);

    *sv_out = sv_lpop_until_predicate(&src_copy, not_number_predicate);

    loc_out->filename = l->filename;
    loc_out->line     = l->line;
    loc_out->col      = col(l);

    if (src_copy.data[0] == '.') {
        String_view dot_sv = sv_lpop(&src_copy, 1);

        /*info("dot_sv: '"SV_FMT"'", SV_ARG(dot_sv));*/

        String_view float_sv = sv_lpop_until_predicate(&src_copy, not_number_predicate);

        /*info("float_sv: '"SV_FMT"'", SV_ARG(float_sv));*/

        // NOTE: We can do this because dot_sv and float_sv is right after sv_out!
        sv_out->count += dot_sv.count + float_sv.count;
    }
    
    // Advance by the len of sv
    l->cur += sv_out->count;
}

void consume_comment(Lexer *l, String_view *sv_out, Location *loc_out) {
    ASSERT(current_char(l) == '/', "We expect a comment to start with '/'...");

    // 0 means its after EOF
    char next = next_char(l);
    consume_char(l);

    switch (next) {
        case 0:
        case '\n': {
            Location loc = {
                .filename = l->filename,
                .line = l->line,
                .col = col(l),
            };
            compiler_error(loc, "Unterminated comment!");
            exit(1);
        } break;
        case '/': {
            // Eat /
            consume_char(l);

            String_view src_copy = get_src_copy(l);

            *sv_out = sv_lpop_until_char(&src_copy, '\n');

            loc_out->filename = l->filename;
            loc_out->line     = l->line;
            loc_out->col      = col(l);

            // Advance by the len of sv
            l->cur += sv_out->count;

        } break;
        case '*': {
            // Eat *
            consume_char(l);

            String_view src_copy = get_src_copy(l);

            *sv_out = sv_lpop_until_string(&src_copy, "*/");

            loc_out->filename = l->filename;
            loc_out->line     = l->line;
            loc_out->col      = col(l);

            // Eat */
            consume_char(l);
            consume_char(l);

            // Advance by the len of sv
            l->cur += sv_out->count;
        } break;
        default: {
            ASSERT(false, "This shouldnt happen; if it did, you fucked up");
        } break;
    }
}

void left_trim(Lexer *l) {
    while (!eof(l) && isspace(current_char(l))) {
        if (current_char(l) == '\n' || (current_char(l) == '\r' && next_char(l) == '\n')) {
            l->line += 1;
            l->bol = l->cur + 1;
        }
        consume_char(l);
    }
}

#define LEX_N_CHAR_TOKEN(token_type, lexeme_len) \
    t_out->lexeme = (String_view) {\
        .data = &(l->src.data[l->cur]),\
        .count = lexeme_len,\
    };\
    t_out->type = token_type;\
    t_out->loc = (Location) {\
        .filename = l->filename,\
        .line = l->line,\
        .col = col(l),\
    };\
    if (DEBUG_PRINT) {\
        print_token(stdout, *t_out);\
        putc('\n', stdout);\
    }\
    for (int i = 0; i < lexeme_len; ++i) {\
        consume_char(l);\
    }\
    return true

bool next_token(Lexer *l, Token *t_out) {
    left_trim(l);

    if (eof(l)) return false;

    char ch = current_char(l);

    if (isalpha(ch) || ch == '_') {
        String_view ident_sv = {0};
        Location ident_loc = {0};
        consume_ident(l, &ident_sv, &ident_loc);

        t_out->lexeme = ident_sv;
        t_out->loc    = ident_loc;
        t_out->type   = (is_keyword(ident_sv) ? TK_KEYWORD : TK_IDENT);
        if (DEBUG_PRINT) {
            print_token(stdout, *t_out);
            putc('\n', stdout);
        }
        return true;
    }

    if (isdigit(ch)) {
        String_view number_sv = {0};
        Location number_loc = {0};

        consume_number(l, &number_sv, &number_loc);

        t_out->lexeme = number_sv;
        t_out->loc    = number_loc;
        t_out->type   = number_token_type(number_sv);
        if (DEBUG_PRINT) {
            print_token(stdout, *t_out);
            putc('\n', stdout);
        }
        return true;
    }

    switch (ch) {
        case '/': {
            // / could be // /**/ or /=

            char next = next_char(l);

            switch (next) {
                case '*': 
                case '/': {
                    String_view comment_sv = {0};
                    Location comment_loc = {0};

                    consume_comment(l, &comment_sv, &comment_loc);

                    t_out->lexeme = comment_sv;
                    t_out->loc    = comment_loc;
                    t_out->type   = comment_token_type(comment_sv);
                    if (DEBUG_PRINT) {
                        print_token(stdout, *t_out);
                        putc('\n', stdout);
                    }

                    return true;
                } break;
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_DIVIDE_EQUAL, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_DIVIDE, 1);
        } break;
        case '"': {
            String_view string_sv = {0};
            Location string_loc = {0};
            consume_string(l, &string_sv, &string_loc);

            t_out->lexeme = string_sv;
            t_out->loc    = string_loc;
            t_out->type   = TK_STRING;
            if (DEBUG_PRINT) {
                print_token(stdout, *t_out);
                putc('\n', stdout);
            }

            return true;
        } break;
        case ':': {
            LEX_N_CHAR_TOKEN(TK_COLON, 1);
        } break;
        case ';': {
            LEX_N_CHAR_TOKEN(TK_SEMICOLON, 1);
        } break;
        case '#': {
            LEX_N_CHAR_TOKEN(TK_HASH, 1);
        } break;
        case '(': {
            LEX_N_CHAR_TOKEN(TK_LEFT_PAREN, 1);
        } break;
        case ')': {
            LEX_N_CHAR_TOKEN(TK_RIGHT_PAREN, 1);
        } break;
        case '{': {
            LEX_N_CHAR_TOKEN(TK_LEFT_BRACE, 1);
        } break;
        case '}': {
            LEX_N_CHAR_TOKEN(TK_RIGHT_BRACE, 1);
        } break;
        case '[': {
            LEX_N_CHAR_TOKEN(TK_LEFT_SQUARE_BRACE, 1);
        } break;
        case ']': {
            LEX_N_CHAR_TOKEN(TK_RIGHT_SQUARE_BRACE, 1);
        } break;
        case '-': {
            // - could be --, -= or ->
            char next = next_char(l);

            switch (next) {
                case '-': {
                    LEX_N_CHAR_TOKEN(TK_MINUS_MINUS, 2);
                } break;
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_MINUS_EQUAL, 2);
                } break;
                case '>': {
                    LEX_N_CHAR_TOKEN(TK_RETURNER, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_MINUS, 1);
        } break;
        case '+': {
            // + could be ++ or +=
            char next = next_char(l);

            switch (next) {
                case '+': {
                    LEX_N_CHAR_TOKEN(TK_PLUS_PLUS, 2);
                } break;
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_PLUS_EQUAL, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_PLUS, 1);
        } break;
        case '*': {
            // * could be ** or *=
            char next = next_char(l);

            switch (next) {
                case '*': {
                    LEX_N_CHAR_TOKEN(TK_POWER, 2);
                } break;
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_MULTIPLY_EQUAL, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_MULTIPLY, 1);
        } break;
        case '%': {
            // % could be %=
            char next = next_char(l);

            switch (next) {
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_MODULUS_EQUAL, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_MODULUS, 1);
        } break;
        case '&': {
            // & could be && or &=
            char next = next_char(l);

            switch (next) {
                case '&': {
                    LEX_N_CHAR_TOKEN(TK_LOGICAL_AND, 2);
                } break;
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_BITWISE_AND_EQUAL, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_BITWISE_AND, 1);
        } break;
        case '^': {
            // ^ could be ^=
            char next = next_char(l);

            switch (next) {
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_BITWISE_XOR_EQUAL, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_BITWISE_XOR, 1);
        } break;
        case '~': {
            LEX_N_CHAR_TOKEN(TK_BITWISE_NOT, 1);
        } break;
        case '|': {
            // | could be || or |=
            char next = next_char(l);

            switch (next) {
                case '|': {
                    LEX_N_CHAR_TOKEN(TK_LOGICAL_OR, 2);
                } break;
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_BITWISE_OR_EQUAL, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_BITWISE_OR, 1);
        } break;
        case '!': {
            // ! could be !=
            char next = next_char(l);

            switch (next) {
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_NOT_EQUAL, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_NOT, 1);
        } break;
        case '=': {
            // = could be ==
            char next = next_char(l);

            switch (next) {
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_EQUAL_EQUAL, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_EQUAL, 1);
        } break;
        case '<': {
            // < could be << or <=
            char next = next_char(l);

            switch (next) {
                case '<': {
                    LEX_N_CHAR_TOKEN(TK_BITWISE_SHIFT_LEFT, 2);
                } break;
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_LTE, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_LT, 1);
        } break;
        case '>': {
            // > could be >> or >=
            char next = next_char(l);

            switch (next) {
                case '>': {
                    LEX_N_CHAR_TOKEN(TK_BITWISE_SHIFT_RIGHT, 2);
                } break;
                case '=': {
                    LEX_N_CHAR_TOKEN(TK_GTE, 2);
                } break;
            }

            LEX_N_CHAR_TOKEN(TK_GT, 1);
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

typedef struct {
    const char **items;
    size_t count;
    size_t capacity;
} Flags;

int main(int argc, char **argv) {
    const char *program = shift_args(argv, argc);

    Flags flags = {0};

    const char *filename = NULL;
    while (argc > 0) {
        const char *arg = shift_args(argv, argc);

        if (*arg == '-' || *arg == '/') {
            const char *flag = arg; // Including the prefix

            da_append(flags, flag);
        } else {
            filename = arg;
        }
    }

    // Parse flags
    for (size_t i = 0; i < flags.count; ++i) {
        const char *flag = flags.items[i];

        char prefix = *flag;

        flag += 1; // Remove prefix

        if (strcmp(flag, "h") == 0) {
            help(program);
            exit(0);
        } else if (strcmp(flag, "v") == 0) {
            info("Compiler Version: "COMPILER_VERSION);
            exit(0);
        } else if (strcmp(flag, "d") == 0) {
            DEBUG_PRINT = true;
        } else {
            error("Invalid flag '%c%s'...", prefix, flag);
            exit(1);
        }
    }

    if (filename == NULL) {
        error("Please provide a filename!");
        usage(program);
        da_free(flags);
        return 1;
    }

    Lexer l = make_lexer(filename);

    Tokens tokens = lex(&l);

    Parser p = make_parser(tokens);

    free_parser(&p);
    free_lexer(&l);
    da_free(flags);
    return 0;
}
