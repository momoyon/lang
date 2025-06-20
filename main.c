#include <stdio.h>
#include <ctype.h>

#define COMMONLIB_IMPLEMENTATION
#define COMMONLIB_REMOVE_PREFIX
#include <commonlib.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define STB_SPRINTF_IMPLEMENTATION
#include "stb_sprintf.h"

#define error log_error
#define info log_info

#if DEBUG
// #error DEBUG IS ENABLED!
#define log_debug(fmt, ...) do {\
		fprintf(stdout, "%s"fmt"\n", "[DEBUG] ", ##__VA_ARGS__);\
	} while (0)
#else
// #error DEBUG IS DISABLED!
#define log_debug(...)
#endif

#define COMPILER_VERSION "v0.0.8"

static bool DEBUG_PRINT = false;

// TODO:Implement every expr parsing for C:
// ast             -> equality ;
// equality        -> comparison ( ( "!=" | "==" ) comparison )* ;
// comparison      -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term            -> factor ( ( "-" | "+" ) factor )* ;
// factor          -> unary ( ( "/" | "*" ) unary )* ;
// cast            -> "(" TYPE ")" ast ;
// unary_not       -> ( "!" | "~" ) ( unary | primary ) ;
// unary_term      -> ( "-" | "+" ) ( unary | primary ) ;
// prefix          -> ( "++" | "--" ) IDENT ;
// comp.lit        -> Skipped...
// // TODO: These all IDENT need to be ast, but if i do that, how can i parse them?
// I will need to call parse() the first thing and check the next token... Which i cant really do right now that easily
// access          -> ( IDENT | funcalls ) "." ( access | IDENT ) ;
// subscript       -> IDENT "[" ast "]" ;
// funcalls        -> IDENT "(" ( ast "," )* ")" | IDENT "(" ast ")" ;
// suffix          -> IDENT ( "++" | "--" ) ;
// primary         -> NUMBER | STRING | IDENT | "true" | "false" | "null" | "(" ast ")" ;

/* NOTE: We are referencing this table: https://en.cppreference.com/w/c/language/operator_precedence
 * PRECEDENCE TABLE
 *
 * LOW
 *  |
 *  v
 * HIGH
 *
 * NAME                | OP                                  | ASSOCIATE | DONE
 * --------------------+-------------------------------------+-----------+-----
 * Comma               | ,                                   | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Bitwise Assignment  | &= |= ^=                            | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * Bitshift Assignment | <<= >>=                             | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * Factor Assignment   | /= *= %=                            | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * Term Assignment     | += -=                               | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * Simple Assignment   | =                                   | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * Ternary Condition   | ?:                                  | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * Logical OR          | ||                                  | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Logical AND         | &&                                  | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Bitwise OR          | |                                   | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Bitwise XOR         | ^                                   | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Bitwise AND         | &                                   | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Equality            | == !=                               | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Comparision         | > >= < <=                           | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Bit shift           | << >>                               | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Term                | - +                                 | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * Factor              | / * %                               | Left      |
 * --------------------+-------------------------------------+-----------+-----
 * sizeof              | sizeof                              | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * Address-of          | &                                   | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * Dereference         | *                                   | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * Cast                | (type)                              | Right     |
 * --------------------+-------------------------------------+-----------+-----
 * L/B NOT             | ! ~                                 | Right     | X
 * --------------------+-------------------------------------+-----------+-----
 * Unary Plus/Minus    | + -                                 | Right     | X
 * --------------------+-------------------------------------+-----------+-----
 * Prefix Inc/Dec      | ++ --                               | Right     | X
 * --------------------+-------------------------------------+-----------+-----
 * Compound Lit        | (type){list}                        | Left      |     // TODO: We skipped this
 * --------------------+-------------------------------------+-----------+-----
 * Struct/Union access | .                                   | Left      | X    NOTE: We use . to access through pointers as well
 * --------------------+-------------------------------------+-----------+-----
 * Array Subscripting  | []                                  | Left      | X
 * --------------------+-------------------------------------+-----------+-----
 * Function Call       | ()                                  | Left      | X
 * --------------------+-------------------------------------+-----------+-----
 * Suffix Inc/Dec      | ++ --                               | Left      | X
 * --------------------+-------------------------------------+-----------+-----
 * Primary             | IDENTS NUMBERS STRINGS CHARS (ast)  | -         | X
 * --------------------+-------------------------------------+-----------+-----
 */


/// NOTE: Location
typedef struct {
    const char *filename;
    int line; // NOTE: Lines start from 1!!!!!!!
    int col;
} Location;

void print_loc(FILE *f, Location loc);

///

/// NOTE: Error
#define ERROR_BUF_CAP (1024)
typedef struct {
    char buf[ERROR_BUF_CAP];
    Location loc;
} Error;

///

/// NOTE: Token

typedef enum {
    TK_IDENT,
    TK_KEYWORD,
    TK_TYPE,

    TK_COMMENT,
    TK_MULTILINE_COMMENT,

    TK_STRING,
    TK_CHAR,

    TK_BOOL,
    TK_NULL,

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

    TK_EOF,
    TK_COUNT,
} Token_type;

const char *token_type_as_str(Token_type t);

typedef struct {
    String_view lexeme;
    Location loc;
    Token_type type;
    bool dummy;
} Token;

Token dummy_token(void);
bool token_is_number(Token t);
void print_token(FILE *f, Token t);

typedef struct {
    Token *items;
    size_t count;
    size_t capacity;
} Tokens;

///

/// NOTE: Lexer

typedef struct {
    size_t bol;
    size_t eol;
} Line;

typedef struct {
    Line *items;
    size_t count;
    size_t capacity;
} Lines;

typedef struct {
    // NOTE: src gets data from a heap allocated string!!!
    String_view src;
    size_t cur;
    size_t bol; // Beginning of Line
    size_t current_line;
    Lines lines;
    const char *filename;
} Lexer;

Lexer make_lexer(const char *filename);
Tokens lex(Lexer *l);
void init_lexer_lines(Lexer *l);
bool next_token(Lexer *l, Token *t_out, Error *err);
String_view get_src_copy(Lexer *l);
bool eof(Lexer *l);
int col(Lexer *l);
char current_char(Lexer *l);
char next_char(Lexer *l);
char consume_char(Lexer *l);
bool consume_ident(Lexer *l, String_view *ident_sv_out, Location *loc_out, Error *err);
bool consume_string(Lexer *l, String_view *string_sv_out, Location *loc_out, Error *err);
bool consume_character(Lexer *l, String_view *char_sv_out, Location *loc_out, Error *err);
bool consume_single_char(Lexer *l, String_view *sv_out, Location *loc_out, Error *err);
bool consume_number(Lexer *l, String_view *sv_out, Location *loc_out, Error *err);
bool consume_comment(Lexer *l, String_view *sv_out, Location *loc_out, Error *err);
void left_trim(Lexer *l);
void free_lexer(Lexer *l);

///

/// NOTE: Parser
typedef struct {
    Tokens tokens;
    int current_token_id;
    Lexer *lexer;
} Parser;

Parser make_parser(Lexer *lexer, Tokens tokens);
bool parser_match(Parser *p, const Token_type t);
bool parser_check_token(Parser *p, const Token_type t);
Token parser_advance(Parser *p);
Token parser_previous(Parser *p);
Token parser_peek_by(Parser *p, int by);
Token parser_peek(Parser *p);
bool parser_eof(Parser *p);
void free_parser(Parser *p);

///

/// NOTE: ASTs and Expressions
typedef struct Literal Literal;
typedef enum   Literal_kind Literal_kind;
typedef struct Binary_expr Binary_expr;
typedef struct Funcall_AST Funcall_AST;
typedef struct Subscript_AST Subscript_AST;
typedef struct Access_AST Access_AST;
typedef struct Unary_term_AST Unary_term_AST;
typedef struct Unary_not_AST Unary_not_AST;
typedef struct Cast_AST Cast_AST;
typedef struct Suffix_AST Suffix_AST;
typedef struct Prefix_AST Prefix_AST;
typedef struct Primary_expr Primary_expr;
typedef struct AST AST;
typedef enum   Primary_expr_kind Primary_expr_kind;

enum Literal_kind {
    LIT_FLOAT,
    LIT_INT,
    LIT_BOOL,
    LIT_CHAR,
    LIT_STRING,
    LIT_COUNT,
};

const char *lit_kind_as_str(Literal_kind k);

struct Literal {
    Literal_kind kind;
    union {
        float f;
        int i;
        bool b;
        char ch;
        char *str;
    } as;
};

void print_literal(FILE *f, Literal value);

struct Unary_term_AST {
    Token operator;
    AST *operand;
};

struct Unary_not_AST {
    Token operator;
    AST *operand;
};

struct Cast_AST {
    Token type; // Type we are casting to
    AST *ast;   // Ast we are casting
};

typedef struct {
    AST **items;
    size_t count;
    size_t capacity;
} ASTs;

struct Funcall_AST {
    Token name;
    ASTs arguments;
};

struct Subscript_AST {
    String_view identifier_key;
    AST *index_ast;
};

typedef enum {
    ACC_RHS_ACCESS,
    ACC_RHS_IDENT,
    ACC_RHS_COUNT,
} Access_AST_rhs_kind;

struct Access_AST {
    Token lhs; // Eg: foo.bar // here `foo` is lhs
    union {
        AST *access;
        String_view ident_key;
    } rhs_as;
    Access_AST_rhs_kind rhs_kind;
};

struct Binary_expr {
    Token operator;
    AST *lhs;
    AST *rhs;
};

struct Suffix_AST {
    Token ident;
    AST *operator;
};

struct Prefix_AST {
    Token ident;
    AST *operator;
};

enum Primary_expr_kind {
    PRIMARY_VALUE,
    PRIMARY_IDENT,
    PRIMARY_COUNT,
};

struct Primary_expr {
    Primary_expr_kind kind;
    Literal value;
    String_view identifier_key;
};

void print_primary_expr(FILE *f, Primary_expr *pe);

typedef enum {
    AST_INVALID,
    AST_PRIMARY,
    AST_SUFFIX,
    AST_FUNCALL,
    AST_SUBSCRIPT,
    AST_ACCESS,
    AST_PREFIX,
    AST_UNARY_TERM,
    AST_UNARY_NOT,
    AST_CAST,
    AST_BINARY,
    AST_COUNT,
} AST_kind;

const char *ast_kind_as_str(AST_kind k);

struct AST {
    AST_kind kind;
    Primary_expr    *prim_expr;
    Suffix_AST      *suffix;
    Funcall_AST     *funcall;
    Subscript_AST   *subscript;
    Access_AST      *access;
    Prefix_AST      *prefix;
    Unary_term_AST  *unary_term;
    Unary_not_AST   *unary_not;
    Cast_AST        *cast;
    Binary_expr     *bin_expr;
    Location loc;
};

void print_ast_as_value(FILE *f, AST e);
void print_ast(FILE *f, AST e);

typedef struct {
    AST **items;
    size_t count;
    size_t capacity;
} AST_refs;

///

/// Identifiers

typedef struct {
    String_view name;
    Literal value;
    bool not_declared;
    Primary_expr *prim_expr;
} Identifier;

typedef struct {
    String_view key;
    Identifier value;
} Identifier_KV;

static Identifier_KV *identifier_map = {0};

///

static void usage(const char *program) {
    info("Usage: %s [subcommand] [flag(s)] <file>", program);
}

static void help(const char *program) {
    usage(program);

    info("");
    info("Flags: ");
    info("  -h                Prints this help message.");
    info("  -v                Prints the version of the compiler.");
    info("Subcommands: ");
    info("  help              Prints this help message.");
    info("  dump_tokens       Dumps the tokens lexed and exit.");
    info("  dump_ast          Dumps the ASTs parsed and exit.");
}

void print_loc(FILE *f, Location loc) {
    /*ASSERT(loc != NULL, "Bro you passed a NULL...");*/
    fprintf(f, "%s:%d:%d", loc.filename, loc.line, loc.col);
}

#define error_pretty(loc, lexer, fmt, ...) do {\
        print_loc(stderr, loc);\
        putc(' ', stderr);\
        ASSERT(0 <= ((loc).line-1) && (size_t)((loc).line-1) <= (lexer).lines.count-1, "Should be in range");\
        Line line = (lexer).lines.items[(loc).line-1];\
        String_view line_sv = sv_get_part((lexer).src, line.bol, line.eol);\
        error(fmt, ##__VA_ARGS__);\
        printf(SV_FMT"\n", SV_ARG(line_sv));\
        printf("%*s^\n", (loc).col, "");\
    } while (0)

Token dummy_token(void) {
    Token t = {0};
    t.dummy = true;
    return t;
}

bool token_is_number(Token t) {
    return t.type == TK_INT || t.type == TK_FLOAT;
}

void print_literal(FILE *f, Literal value) {
    switch (value.kind) {
        case LIT_FLOAT:  fprintf(f, "%f", value.as.f); break;
        case LIT_INT:    fprintf(f, "%d", value.as.i); break;
        case LIT_BOOL:   fprintf(f, "%s", value.as.b ? "true" : "false"); break;
        case LIT_CHAR:   fprintf(f, "'%c'", value.as.ch); break;
        case LIT_STRING: fprintf(f, "\"%s\"", value.as.str); break;
        case LIT_COUNT:
        default: ASSERT(false, "UNREACHABLE!");
    }
}

const char *lit_kind_as_str(Literal_kind k) {
    switch (k) {
        case LIT_FLOAT: return "FLOAT";
        case LIT_INT: return "INT";
        case LIT_BOOL: return "BOOL";
        case LIT_CHAR: return "CHAR";
        case LIT_STRING: return "STRING";
        case LIT_COUNT:
        default: ASSERT(false, "UNREACHABLE!");
    }
    return "YOU SHOULD NOT SEE THIS!";
}

const char *expr_kind_as_str(AST_kind k) {
    switch (k) {
        case AST_INVALID: return "INVALID";
        case AST_PRIMARY: return "PRIMARY";
        case AST_SUFFIX: return "SUFFIX";
        case AST_FUNCALL: return "FUNCALL";
        case AST_SUBSCRIPT: return "SUBSCRIPT";
        case AST_ACCESS: return "ACCESS";
        case AST_PREFIX: return "PREFIX";
        case AST_UNARY_TERM: return "UNARY_TERM";
        case AST_UNARY_NOT: return "UNARY_NOT";
        case AST_CAST: return "CAST";
        case AST_BINARY: return "BINARY";
        case AST_COUNT:
        default: ASSERT(false, "UNREACHABLE!");
    }

    return "YOU SHOULD NOT SEE THIS!";
}

void print_primary_expr(FILE *f, Primary_expr *pe) {
    if (pe->kind == PRIMARY_VALUE) {
        print_literal(f, pe->value);
    } else if (pe->kind == PRIMARY_IDENT) {
        Identifier_KV *ident_kv = hmgetp_null(identifier_map, pe->identifier_key);
        ASSERT(ident_kv != NULL, "The identifier should be in the identifier_map!");
        Identifier ident = ident_kv->value;

        fprintf(f, "'"SV_FMT"': ", SV_ARG(ident.name));
        if (ident.not_declared) {
            fprintf(f, "???");
        } else {
            print_literal(f, ident.value);
        }
    } else {
        ASSERT(false, "UNREACHABLE!");
    }
}

void print_ast_as_value(FILE *f, AST e) {
    switch (e.kind) {
        case AST_INVALID: {
            ASSERT(false, "Trying to print an invalid AST!");
        } break;
        case AST_BINARY: {
            fprintf(f, "(");
            print_ast_as_value(f, *e.bin_expr->lhs);
            fprintf(f, " %s ", token_type_as_str(e.bin_expr->operator.type));
            print_ast_as_value(f, *e.bin_expr->rhs);
            fprintf(f, ")");

        } break;
        case AST_UNARY_TERM: {
             fprintf(f, " %s ", token_type_as_str(e.unary_term->operator.type));
             print_ast_as_value(f, *e.unary_term->operand);
        } break;
        case AST_UNARY_NOT: {
             fprintf(f, " %s ", token_type_as_str(e.unary_not->operator.type));
             print_ast_as_value(f, *e.unary_not->operand);
        } break;
        case AST_CAST: {
             fprintf(f, "( "SV_FMT" ) ", SV_ARG(e.cast->type.lexeme));
             print_ast_as_value(f, *e.cast->ast);
        } break;
        case AST_PRIMARY: {
            print_primary_expr(f, e.prim_expr);
        } break;
        case AST_SUFFIX: {
            fprintf(f, SV_FMT, SV_ARG(e.suffix->ident.lexeme));
            print_ast_as_value(f, *e.suffix->operator);
        } break;
        case AST_FUNCALL: {
            fprintf(f, SV_FMT, SV_ARG(e.funcall->name.lexeme));
            fprintf(f, "(");
            for (size_t i = 0; i < e.funcall->arguments.count; ++i) {
                print_ast_as_value(f, *e.funcall->arguments.items[i]);
                if (i != e.funcall->arguments.count-1) fprintf(f, ", ");
            }
            fprintf(f, ")");
        } break;
        case AST_SUBSCRIPT: {
            fprintf(f, SV_FMT, SV_ARG(e.subscript->identifier_key));
            fprintf(f, "[");
            print_ast_as_value(f, *e.subscript->index_ast);
            fprintf(f, "]");
        } break;
        case AST_ACCESS: {
            fprintf(f, SV_FMT, SV_ARG(e.access->lhs.lexeme));
            fprintf(f, ".");
            switch (e.access->rhs_kind) {
                case ACC_RHS_ACCESS: {
                    print_ast_as_value(f, *e.access->rhs_as.access);
                } break;
                case ACC_RHS_IDENT: {
                    fprintf(f, SV_FMT, SV_ARG(e.access->rhs_as.ident_key));
                } break;
                case ACC_RHS_COUNT:
                default:
                    ASSERT(false, "UNREACHABLE!");
            }
        } break;
        case AST_PREFIX: {
            print_ast_as_value(f, *e.prefix->operator);
            fprintf(f, SV_FMT, SV_ARG(e.prefix->ident.lexeme));
        } break;

        case AST_COUNT:
        default: ASSERT(false, "UNREACHABLE!");
    }
}

void print_ast(FILE *f, AST e) {
    print_loc(f, e.loc);
    fprintf(f, " [%s] '", expr_kind_as_str(e.kind));
    print_ast_as_value(f, e);
    fprintf(f, "'");
}

const char *token_type_as_str(Token_type t) {
    switch (t) {
        case TK_IDENT: return "IDENT";
        case TK_KEYWORD: return "KEYWORD";
        case TK_TYPE: return "TYPE";
        case TK_COMMENT: return "COMMENT";
        case TK_MULTILINE_COMMENT: return "MULTILINE_COMMENT";
        case TK_STRING: return "STRING";
        case TK_CHAR: return "CHAR";
        case TK_BOOL: return "BOOL";
        case TK_NULL: return "null";
        case TK_LEFT_PAREN: return "(";
        case TK_RIGHT_PAREN: return ")";
        case TK_MINUS: return "-";
        case TK_MINUS_MINUS: return "--";
        case TK_MINUS_EQUAL: return "-=";
        case TK_PLUS: return "+";
        case TK_PLUS_PLUS: return "++";
        case TK_PLUS_EQUAL: return "+=";
        case TK_RETURNER: return "->";
        case TK_LEFT_BRACE: return "{";
        case TK_RIGHT_BRACE: return "}";
        case TK_DIVIDE: return "/";
        case TK_DIVIDE_EQUAL: return "/=";
        case TK_MULTIPLY: return "*";
        case TK_MULTIPLY_EQUAL: return "*=";
        case TK_MODULUS: return "%";
        case TK_MODULUS_EQUAL: return "%=";
        case TK_POWER: return "**";
        case TK_EQUAL: return "=";
        case TK_NOT: return "!";
        case TK_NOT_EQUAL: return "!=";
        case TK_EQUAL_EQUAL: return "==";
        case TK_GT: return ">";
        case TK_LT: return "<";
        case TK_GTE: return ">=";
        case TK_LTE: return "<=";
        case TK_COMMA: return ",";
        case TK_COLON: return ":";
        case TK_SEMICOLON: return ";";
        case TK_DOT: return ".";
        case TK_HASH: return "#";
        case TK_LEFT_SQUARE_BRACE: return "[";
        case TK_RIGHT_SQUARE_BRACE: return "]";
        case TK_INT: return "INT";
        case TK_FLOAT: return "FLOAT";
        case TK_BITWISE_AND: return "&";
        case TK_BITWISE_AND_EQUAL: return "&=";
        case TK_BITWISE_NOT: return "~";
        case TK_BITWISE_OR: return "|";
        case TK_BITWISE_OR_EQUAL: return "|=";
        case TK_BITWISE_XOR: return "^";
        case TK_BITWISE_XOR_EQUAL: return "^=";
        case TK_BITWISE_SHIFT_LEFT: return "<<";
        case TK_BITWISE_SHIFT_RIGHT: return ">>";
        case TK_LOGICAL_AND: return "&&";
        case TK_LOGICAL_OR: return "||";
        case TK_EOF: return "EOF";
        case TK_COUNT:
        default: {
            ASSERT(false, "UNREACHABLE");
        }
    }
}

typedef struct {
    const char **items;
    size_t count;
    size_t capacity;
} Types;

static Types types = {0};

const char *predefined_types[] = {
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
};

const char *keywords[] = {

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

bool is_type(String_view ident) {
    for (size_t i = 0; i < types.count; ++i) {
        if (sv_equals(ident, SV(types.items[i]))) return true;
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
    if (t.dummy) {
        fprintf(f, "<DUMMY_TOKEN>");
    } else {
        print_loc(f, t.loc);
        fprintf(f, " [%s] '"SV_FMT"'", token_type_as_str(t.type), SV_ARG(t.lexeme));
    }
}

bool parser_match(Parser *p, const Token_type t) {
    if (parser_check_token(p, t)) {
        parser_advance(p);
        return true;
    }
    return false;
}

bool parser_check_token(Parser *p, const Token_type t) {
    if (parser_eof(p)) return false;
    return parser_peek(p).type == t;
}

Token parser_advance(Parser *p) {
    if (!parser_eof(p)) {
        p->current_token_id += 1;
    }
    return parser_previous(p);
}

Token parser_peek_by(Parser *p, int by) {
    if (0 > (int)(p->current_token_id+by) || p->current_token_id+by > (int)p->tokens.count-1) {
        log_debug("parser_peek_by() OUTOFBOUNDS!");
        return dummy_token();
    }
    return p->tokens.items[p->current_token_id+by];
}

Token parser_previous(Parser *p) {
    return parser_peek_by(p, -1);
}

Token parser_peek(Parser *p) {
    return parser_peek_by(p, 0);
}

bool parser_eof(Parser *p) {
    return parser_peek(p).type == TK_EOF;
}

// Token parser_current_token(Parser *p) {
//     ASSERT(0 <= p->current_token_id && (size_t)p->current_token_id <= p->tokens.count - 1, "Parser.current_token_id outofbounds!");
//     return p->tokens.items[p->current_token_id];
// }


// Predecls
AST *parse_primary(Arena *arena, Parser *p);
AST *parse_suffix(Arena *arena, Parser *p);
AST *parse_funcall(Arena *arena, Parser *p);
AST *parse_subscript(Arena *arena, Parser *p);
AST *parse_access(Arena *arena, Parser *p);
AST *parse_prefix(Arena *arena, Parser *p);
AST *parse_unary_term(Arena *arena, Parser *p);
AST *parse_unary_not(Arena *arena, Parser *p);
AST *parse_cast(Arena *arena, Parser *p);
AST *parse_factor(Arena *arena, Parser *p);
AST *parse_comparision(Arena *arena, Parser *p);
AST *parse_term(Arena *arena, Parser *p);
AST *parse_equality(Arena *arena, Parser *p);
AST *parse(Arena *arena, Parser *p);

AST *parse_primary(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type != TK_LEFT_PAREN) {
        AST *ast = (AST *)arena_alloc(arena, sizeof(AST));
        ast->kind = AST_PRIMARY;
        ast->loc = t.loc;
        ast->prim_expr = (Primary_expr *)arena_alloc(arena, sizeof(Primary_expr));
        ast->prim_expr->kind = PRIMARY_VALUE;
        parser_advance(p);
        if (t.type == TK_IDENT) {
            Identifier_KV *ident_kv = hmgetp_null(identifier_map, t.lexeme);
            // We found a non-declared identifier being used, add it to the identifier_map marking it as non-declared
            if (ident_kv == NULL) {
                Identifier ident = {0};
                ident.name = t.lexeme;
                ident.not_declared = true;

                hmput(identifier_map, t.lexeme, ident);
                ident_kv = hmgetp_null(identifier_map, t.lexeme);
            }
            ASSERT(ident_kv != NULL, "We fucked something up above!");
            Identifier ident = ident_kv->value;

            ast->prim_expr->identifier_key = t.lexeme;
            if (ident.not_declared) {
                // If the ident is not declared yet, mark the primary_ast it needs to update the value of for later.
                ident_kv->value.prim_expr = ast->prim_expr;
            } else {
                // If the ident is declared, set the value of the expr!
                ast->prim_expr->value = ident.value;
                ast->prim_expr->value.kind = ident.value.kind;
            }
            ast->prim_expr->kind = PRIMARY_IDENT;

            return ast;
        } else if (token_is_number(t)) {
            if (t.type == TK_INT) {
                int i_count = -1;
                int i = sv_to_int(t.lexeme, &i_count, 10);
                ASSERT(i_count != -1, "We made a mistake in lexing of integers!");
                ast->prim_expr->value.as.i = i;
                ast->prim_expr->value.kind = LIT_INT;
                return ast;
            } else if (t.type == TK_FLOAT) {
                int f_count = -1;
                float f = sv_to_float(t.lexeme, &f_count);
                ASSERT(f_count != -1, "We made a mistake in lexing of floats!");
                ast->prim_expr->value.as.f = f;
                ast->prim_expr->value.kind = LIT_FLOAT;
                return ast;
            } else {

            }
        } else if (t.type == TK_STRING) {
            ast->prim_expr->value.kind = LIT_STRING;
            ast->prim_expr->value.as.str = sv_to_cstr(t.lexeme);
            return ast;
        } else if (t.type == TK_CHAR) {
            ast->prim_expr->value.kind = LIT_CHAR;
            ast->prim_expr->value.as.ch = *t.lexeme.data;
            return ast;
        } else if (t.type == TK_BOOL) {
            ast->prim_expr->value.kind = LIT_BOOL;
            ast->prim_expr->value.as.b = sv_equals(t.lexeme, SV("true"));
            return ast;
        } else if (t.type == TK_NULL) {
            ast->prim_expr->value.kind = LIT_INT; // TODO: Should we introduce a LIT_NULL?
            ast->prim_expr->value.as.i = 0;
            return ast;
        } else if (t.type == TK_TYPE) {
            error_pretty(t.loc, (*p->lexer), "We didn't expect a type here");
            return NULL;
        } else {
            printf("Unexpected Token: "), print_token(stdout, t); printf("\n");
            ASSERT(false, "^");
        }
    } else {
        parser_advance(p); // Skip (
        if (parser_match(p, TK_RIGHT_PAREN)) {
            error_pretty(parser_previous(p).loc, (*p->lexer), "Expected expression before )");
            return NULL;
        }
        AST *ast = parse(arena, p);
        if (parser_peek(p).type != TK_RIGHT_PAREN) {
            Token t = parser_peek(p);
            error_pretty(t.loc, (*p->lexer), "Expected ), But got `%s`", token_type_as_str(t.type));
            return NULL;
        }
        parser_advance(p); // Skip )
        return ast;
    }

    error_pretty(t.loc, (*p->lexer), "Expected expr, but got `%s`", token_type_as_str(t.type));
    return NULL;
}

AST *parse_suffix(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_IDENT &&
            (parser_peek_by(p, 1).type == TK_PLUS_PLUS ||
             parser_peek_by(p, 1).type == TK_MINUS_MINUS)) {
        AST *operator = parse_primary(arena, p);
        ASSERT(operator, "We should be able to parse identifiers using parse_primary()!");

        AST *ast = (AST *)arena_alloc(arena, sizeof(AST));
        ast->loc = t.loc;
        ast->suffix = (Suffix_AST *)arena_alloc(arena, sizeof(Suffix_AST));
        ast->kind = AST_SUFFIX;
        ast->suffix->ident = parser_advance(p);
        ast->suffix->operator = operator;
        return ast;
    }

    return parse_primary(arena, p);
}

AST *parse_funcall(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_IDENT && parser_peek_by(p, 1).type == TK_LEFT_PAREN) {
        AST *ast = (AST *)arena_alloc(arena, sizeof(AST));
        ast->loc = t.loc;
        ast->funcall = (Funcall_AST *)arena_alloc(arena, sizeof(Funcall_AST));
        ast->funcall->name = parser_advance(p);
        ast->kind = AST_FUNCALL;
        parser_advance(p); // Skip (
        Token next = parser_peek(p);
        if (next.type == TK_RIGHT_PAREN) {
            parser_advance(p); // Skip )
            // if (parser_peek(p).type == TK_DOT) {
            //     AST *access_ast = (AST *)arena_alloc(arena, sizeof(AST));
            //     access_ast->loc = parser_peek(p).loc;
            //     access_ast->kind = AST_ACCESS;
            //     access_ast->access = (Access_AST *)arena_alloc(arena, sizeof(Access_AST));
            //     access_ast->access->
            // }
            return ast;
        } else {
            while (true) {
                AST *arg_ast = parse(arena, p);

                if (arg_ast == NULL) return NULL;

                da_append(ast->funcall->arguments, arg_ast);

                if (parser_peek(p).type == TK_RIGHT_PAREN) {
                    parser_advance(p); // Skip )
                    return ast;
                } else if (parser_peek(p).type == TK_COMMA) {
                    parser_advance(p); // Skip ,
                    continue;
                }

                // Special error message
                if (parser_peek(p).type == TK_IDENT) {
                    error_pretty(parser_peek(p).loc, (*p->lexer), "Looks like you forgot a , here?");
                } else {
                    error_pretty(parser_peek(p).loc, (*p->lexer), "Expected , or ) after argument but got `%s`", token_type_as_str(parser_peek(p).type));
                }
                return NULL;
            }
        }
        ASSERT(false, "This should be unreachable!");
    }

    return parse_suffix(arena, p);
}

AST *parse_subscript(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_IDENT && parser_peek_by(p, 1).type == TK_LEFT_SQUARE_BRACE) {
        AST *ast = (AST *)arena_alloc(arena, sizeof(AST));
        ast->loc = t.loc;
        ast->subscript = (Subscript_AST *)arena_alloc(arena, sizeof(Subscript_AST));
        ast->kind = AST_SUBSCRIPT;

        ast->subscript->identifier_key = parser_advance(p).lexeme; // Eat IDENT
        parser_advance(p); // Skip [

        AST *index_ast = parse(arena, p);
        if (index_ast == NULL) return NULL;
        ast->subscript->index_ast = index_ast;

        if (!parser_match(p, TK_RIGHT_SQUARE_BRACE)) {
            error_pretty(parser_peek(p).loc, (*p->lexer), "Expected ] but got `%s`", token_type_as_str(parser_peek(p).type));
        }
        return ast;
    }

    return parse_funcall(arena, p);
}

AST *parse_access(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_IDENT && parser_peek_by(p, 1).type == TK_DOT) {
        AST *ast = (AST *)arena_alloc(arena, sizeof(AST));
        AST *current = ast; // Current access ast, we have to track this since access could be nested like `foo.bar.baz`

        while (true) {
            current->loc = parser_peek(p).loc;
            current->access = (Access_AST *)arena_alloc(arena, sizeof(Access_AST));
            current->kind = AST_ACCESS;
            current->access->lhs = parser_advance(p); // Eat IDENT

            ASSERT(parser_match(p, TK_DOT), "We fucked up logic here"); // Skip .

            Token next = parser_peek(p);
            if (next.type == TK_IDENT) {
                Token next_next = parser_peek_by(p, 1);
                if (next_next.type == TK_DOT) {
                    current->access->rhs_kind = ACC_RHS_ACCESS;
                    current->access->rhs_as.access = (AST *)arena_alloc(arena, sizeof(AST));
                    current = current->access->rhs_as.access;
                    continue;
                } else {
                    current->access->rhs_kind = ACC_RHS_IDENT;
                    current->access->rhs_as.ident_key = next.lexeme;
                    parser_advance(p); // Eat IDENT
                    return ast;
                }
            }
            error_pretty(next.loc, (*p->lexer), "Expected identifier after . but got `%s`", token_type_as_str(next.type));
            return NULL;
        }

        ASSERT(false, "UNREACHABLE!");
    }

    return parse_subscript(arena, p);
}

AST *parse_prefix(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_PLUS_PLUS ||
            t.type == TK_MINUS_MINUS) {

        AST *ast = (AST *)arena_alloc(arena, sizeof(AST));
        ast->loc = t.loc;
        ast->prefix = (Prefix_AST *)arena_alloc(arena, sizeof(Prefix_AST));
        ast->kind = AST_PREFIX;
        ast->prefix->ident = parser_advance(p);
        ast->prefix->operator = parse_primary(arena, p);
        ASSERT(ast->prefix->operator, "We should be able to parse identifiers using parse_primary()!");
        return ast;
    }

    return parse_access(arena, p);
}

AST *parse_unary_term(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_MINUS || t.type == TK_PLUS) {
        AST *ast = (AST *)arena_alloc(arena, sizeof(AST));
        ast->loc = t.loc;
        ast->unary_term = (Unary_term_AST *)arena_alloc(arena, sizeof(Unary_term_AST));
        ast->kind = AST_UNARY_TERM;
        ast->unary_term->operator = parser_advance(p);
        ast->unary_term->operand  = parse_unary_term(arena, p);
        return ast;
    }

    return parse_prefix(arena, p);
}

AST *parse_unary_not(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_NOT || t.type == TK_BITWISE_NOT) {
        AST *ast = (AST *)arena_alloc(arena, sizeof(AST));
        ast->loc = t.loc;
        ast->unary_not = (Unary_not_AST *)arena_alloc(arena, sizeof(Unary_not_AST));
        ast->kind = AST_UNARY_NOT;
        ast->unary_not->operator = parser_advance(p);
        ast->unary_not->operand = parse_unary_not(arena, p);
        return ast;
    }

    return parse_unary_term(arena, p);
}

AST *parse_cast(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_LEFT_PAREN && parser_peek_by(p, 1).type == TK_TYPE) {
        parser_advance(p); // Skip (
        AST *ast = (AST *)arena_alloc(arena, sizeof(AST));
        ast->loc = t.loc;
        ast->cast = (Cast_AST *)arena_alloc(arena, sizeof(Cast_AST));
        ast->kind = AST_CAST;
        ast->cast->type = parser_advance(p);

        if (!parser_match(p, TK_RIGHT_PAREN)) {
            error_pretty(parser_peek(p).loc, (*p->lexer), "Expected ) after `"SV_FMT"` but got `%s`",
                    SV_ARG(ast->cast->type.lexeme),
                    token_type_as_str(parser_peek(p).type));
            return NULL;
        }

        ast->cast->ast  = parse(arena, p);


        return ast;
    }

    return parse_unary_not(arena, p);
}

AST *parse_factor(Arena *arena, Parser *p) {
    AST *ast = parse_cast(arena, p);
    if (ast == NULL) return NULL;

    while (parser_match(p, TK_DIVIDE) || parser_match(p, TK_MULTIPLY)) {
        Token op = parser_previous(p);
        AST *rhs = parse_cast(arena, p);
        if (rhs == NULL) return rhs;

        AST *new_ast = (AST *)arena_alloc(arena, sizeof(AST));
        new_ast->kind = AST_BINARY;
        new_ast->loc = ast->loc;
        new_ast->bin_expr = (Binary_expr *)arena_alloc(arena, sizeof(Binary_expr));
        new_ast->bin_expr->lhs = ast;
        new_ast->bin_expr->operator = op;
        new_ast->bin_expr->rhs = rhs;

        ast = new_ast;
    }

    return ast;
}

AST *parse_term(Arena *arena, Parser *p) {
    AST *ast = parse_factor(arena, p);
    if (ast == NULL) return NULL;
    /*printf("factor ast: %p\n", ast);*/

    while (parser_match(p, TK_MINUS) || parser_match(p, TK_PLUS)) {
        Token operator = parser_previous(p);

        AST *rhs = parse_factor(arena, p);
        if (rhs == NULL) return NULL;

        AST *new_ast = (AST *)arena_alloc(arena, sizeof(AST));
        new_ast->kind = AST_BINARY;
        new_ast->loc = ast->loc;
        new_ast->bin_expr = (Binary_expr *)arena_alloc(arena, sizeof(Binary_expr));
        new_ast->bin_expr->lhs = ast;
        new_ast->bin_expr->operator = operator;
        new_ast->bin_expr->rhs = rhs;

        ast = new_ast;
    }

    return ast;
}

AST *parse_comparision(Arena *arena, Parser *p) {
    AST *ast = parse_term(arena, p);
    if (ast == NULL) return NULL;
    /*printf("term ast: %p\n", ast);*/

    while (parser_match(p, TK_GT) || parser_match(p, TK_GTE) ||
           parser_match(p, TK_LT) || parser_match(p, TK_LTE)) {
        Token operator = parser_previous(p);

        AST *rhs = parse_term(arena, p);
        if (rhs == NULL) return NULL;

        AST *new_ast = (AST *)arena_alloc(arena, sizeof(AST));
        new_ast->kind = AST_BINARY;
        new_ast->loc = ast->loc;
        new_ast->bin_expr = (Binary_expr *)arena_alloc(arena, sizeof(Binary_expr));
        new_ast->bin_expr->lhs = ast;
        new_ast->bin_expr->operator = operator;
        new_ast->bin_expr->rhs = rhs;

        ast = new_ast;
    }

    return ast;
}

AST *parse_equality(Arena *arena, Parser *p) {
    AST *ast = parse_comparision(arena, p);
    if (ast == NULL) return NULL;
    /*printf("comparision ast: %p\n", ast);*/

    while (parser_match(p, TK_NOT_EQUAL) || parser_match(p, TK_EQUAL_EQUAL)) {
        Token operator = parser_previous(p);

        AST *rhs = parse_comparision(arena, p);
        if (rhs == NULL) return NULL;

        AST *new_ast = (AST *)arena_alloc(arena, sizeof(AST));
        new_ast->kind = AST_BINARY;
        new_ast->loc = ast->loc;
        new_ast->bin_expr = (Binary_expr *)arena_alloc(arena, sizeof(Binary_expr));
        new_ast->bin_expr->lhs = ast;
        new_ast->bin_expr->operator = operator;
        new_ast->bin_expr->rhs = rhs;

        ast = new_ast;
    }

    return ast;
}

AST *parse(Arena *arena, Parser *p) {
    AST *ast = parse_equality(arena, p);
    /*printf("equality ast: %p\n", ast);*/
    return ast;
}

Lexer make_lexer(const char *filename) {
    int file_size = -1;
    const char *buf = read_file(filename, &file_size);

    int cr_count = 0;
    for (int i = 0; i < file_size; ++i) {
        if (buf[i] == '\r') {
            cr_count++;
        }
    }
    if (cr_count > 0) {
        log_warning("%d Carriage Returns found!", cr_count);
        exit(1);
    }
    if (file_size == -1) {
        exit(1);
    }
    Lexer l = {
        .src = sv_from_cstr(buf),
        .cur = 0,
        .bol = 0,
        .current_line = 1,
        .filename = filename,
    };

    return l;
}

void free_lexer(Lexer *l) {
    free(l->src.data);
}


/* Understanding BNF [Backus-Naur Form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form):
*
* Example Context-Free-Grammar (CFG):
*
* breakfast -> protein "with" breakfast "on the side" ;
*/


/* GRAMMAR
*   EXPR -> TERM (
*/


Parser make_parser(Lexer *lexer, Tokens tokens) {
    return (Parser) {
        .tokens = tokens,
        .current_token_id = 0,
        .lexer = lexer,
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
    if (eof(l)) return '\0';
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

bool consume_ident(Lexer *l, String_view *ident_sv_out, Location *loc_out, Error *err) {
    (void)err;
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
    loc_out->line     = l->current_line;
    loc_out->col      = col(l);

    // Advance by the len of ident
    l->cur += ident_sv_out->count;

    return true;
}

bool consume_string(Lexer *l, String_view *string_sv_out, Location *loc_out, Error *err) {
    ASSERT(current_char(l) == '"', "We except '\"' to be the current_char here...");

    // Eat "
    consume_char(l);
    String_view src_copy = get_src_copy(l);

    // TODO: Here we parse the string until `"` EVEN IF its on multiple lines...
    // Should we allow this?
    *string_sv_out = sv_lpop_until_char(&src_copy, '"');

    loc_out->filename = l->filename;
    loc_out->line     = l->current_line;
    loc_out->col      = col(l);

    // Advance by the len of sv
    l->cur += string_sv_out->count;

    if (current_char(l) != '"') {
        err->loc = *loc_out;
        stbsp_snprintf(err->buf, ERROR_BUF_CAP, "Unterminted String");
        return false;
    }

    // Eat "
    consume_char(l);

    return true;
}

bool consume_character(Lexer *l, String_view *char_sv_out, Location *loc_out, Error *err) {
    ASSERT(current_char(l) == '\'', "We except '\'' to be the current_char here...");

    // Eat '
    consume_char(l);

    String_view src_copy = get_src_copy(l);

    loc_out->filename = l->filename;
    loc_out->line     = l->current_line;
    loc_out->col      = col(l);

    *char_sv_out = sv_lpop(&src_copy, 1);

    l->cur += 1;

    if (eof(l)) {
        err->loc = *loc_out;
        stbsp_snprintf(err->buf, ERROR_BUF_CAP, "Unterminated char!");
        return false;
    }
    if (current_char(l) != '\'') {
        err->loc = *loc_out;
        char c = current_char(l);
        if (c == '\n')
            stbsp_snprintf(err->buf, ERROR_BUF_CAP, "Expected `'`, but got `EOF`");
        else
            stbsp_snprintf(err->buf, ERROR_BUF_CAP, "Expected `'`, but got `%c`", current_char(l));
        return false;
    }


    // Eat '
    consume_char(l);

    return true;
}

bool consume_single_char(Lexer *l, String_view *sv_out, Location *loc_out, Error *err) {
    (void)err;
    String_view src_copy = get_src_copy(l);

    *sv_out = sv_lpop(&src_copy, 1);

    loc_out->filename = l->filename;
    loc_out->line     = l->current_line;
    loc_out->col      = col(l);

    // Advance by the len of sv
    l->cur += sv_out->count;

    return true;
}

bool consume_number(Lexer *l, String_view *sv_out, Location *loc_out, Error *err) {
    (void)err;
    ASSERT(isdigit(current_char(l)), "We expect a number bro...");

    String_view src_copy = get_src_copy(l);

    *sv_out = sv_lpop_until_predicate(&src_copy, not_number_predicate);

    loc_out->filename = l->filename;
    loc_out->line     = l->current_line;
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

    return true;
}

bool consume_comment(Lexer *l, String_view *sv_out, Location *loc_out, Error *err) {
    ASSERT(current_char(l) == '/', "We expect a comment to start with '/'...");

    // 0 means its after EOF
    char next = next_char(l);
    consume_char(l);

    switch (next) {
        case 0:
        case '\n': {
            Location loc = {
                .filename = l->filename,
                .line = l->current_line,
                .col = col(l),
            };
            err->loc = loc;
            stbsp_snprintf(err->buf, ERROR_BUF_CAP, "Unterminated comment!");
            return false;
        } break;
        case '/': {
            // Eat /
            consume_char(l);

            String_view src_copy = get_src_copy(l);

            *sv_out = sv_lpop_until_char(&src_copy, '\n');

            loc_out->filename = l->filename;
            loc_out->line     = l->current_line;
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
            loc_out->line     = l->current_line;
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

    return true;
}

void left_trim(Lexer *l) {
    while (!eof(l) && isspace(current_char(l))) {
        if (current_char(l) == '\n') l->current_line++;
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
        .line = l->current_line,\
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

void init_lexer_lines(Lexer *l) {
    int nl_count = 0;
    int bol = 0;
    while (!eof(l)) {
        char c = consume_char(l);
        if (c == '\n') {
            nl_count++;
            Line line = {
                .bol = bol,
                .eol = l->cur-1, // NOTE: l->cur is the _NEXT_ char here because we consume the char above!!!
            };

            // log_debug("L %zu ~ %zu", line.bol, line.eol);
            da_append(l->lines, line);

            bol = line.eol + 1;
        }
    }
    // log_debug("Newline count: %d", nl_count);
    // log_debug("lines count: %zu", l->lines.count);

    // log_debug("SRC: `"SV_FMT"`", SV_ARG(l->src));
    // for (size_t i = 0; i < l->lines.count; ++i) {
    //     char eol = l->src.data[l->lines.items[i].eol];
    //     if (eol != '\n') log_debug("EOL expected but got: %c", eol);
    //     ASSERT(eol == '\n', "This should be a newline!");
    //     String_view line_sv = sv_get_part(l->src, l->lines.items[i].bol, l->lines.items[i].eol);
    //     log_debug("L%zu: `"SV_FMT"`", i, SV_ARG(line_sv));
    // }

    // NOTE: Reset the cursor so we can lex
    ASSERT(nl_count == l->lines.count, "Newline count and lines count should match!");
    l->cur = 0;
}

bool next_token(Lexer *l, Token *t_out, Error *err) {
    left_trim(l);

    if (eof(l)) return false;

    char ch = current_char(l);

    if (isalpha(ch) || ch == '_') {
        String_view ident_sv = {0};
        Location ident_loc = {0};
        consume_ident(l, &ident_sv, &ident_loc, err);

        t_out->lexeme = ident_sv;
        t_out->loc    = ident_loc;
        t_out->type   = (is_keyword(ident_sv) ? TK_KEYWORD : is_type(ident_sv) ? TK_TYPE : TK_IDENT);
        if (sv_equals(ident_sv, SV("true")) || sv_equals(ident_sv, SV("false"))) {
            t_out->type = TK_BOOL;
        } else if (sv_equals(ident_sv, SV("null"))) {
            t_out->type = TK_NULL;
        }
        if (DEBUG_PRINT) {
            print_token(stdout, *t_out);
            putc('\n', stdout);
        }
        return true;
    }

    if (isdigit(ch)) {
        String_view number_sv = {0};
        Location number_loc = {0};

        consume_number(l, &number_sv, &number_loc, err);

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

                    consume_comment(l, &comment_sv, &comment_loc, err);

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
            consume_string(l, &string_sv, &string_loc, err);

            t_out->lexeme = string_sv;
            t_out->loc    = string_loc;
            t_out->type   = TK_STRING;
            if (DEBUG_PRINT) {
                print_token(stdout, *t_out);
                putc('\n', stdout);
            }

            return true;
        } break;
        case '\'': {
            String_view char_sv = {0};
            Location char_loc = {0};
            consume_character(l, &char_sv, &char_loc, err);
            t_out->lexeme = char_sv;
            t_out->loc    = char_loc;
            t_out->type   = TK_CHAR;

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
        case ',': {
            LEX_N_CHAR_TOKEN(TK_COMMA, 1);
        } break;
        case '.': {
            LEX_N_CHAR_TOKEN(TK_DOT, 1);
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
    Error err = {0};

    while (next_token(l, &t, &err)) {
        da_append(tokens, t);
    }

    Line last_line = l->lines.items[l->lines.count-1];
    t.loc.col = last_line.eol - last_line.bol;
    t.loc.line = l->current_line-1;
    t.lexeme = SV("EOF");
    t.loc.filename = l->filename;
    t.type = TK_EOF;
    da_append(tokens, t);

    if (*err.buf != '\0') {
        error_pretty(err.loc, *l, "%s", err.buf);
        exit(1);
    }

    return tokens;
}

typedef struct {
    const char **items;
    size_t count;
    size_t capacity;
} Flags;

void init(void) {

    // Append predefined types to global types array
    for (size_t i = 0; i < ARRAY_LEN(predefined_types); ++i) {
        da_append(types, predefined_types[i]);
    }

    // for (size_t i = 0; i < types.count; ++i) {
    //     log_debug("Type: %s", types.items[i]);
    // }
}

int main(int argc, char **argv) {
    const char *program = shift_args(argv, argc);

    const char *filename = NULL;

    bool dump_tokens = false;
    bool dump_ast = false;

    while (argc > 0) {
        const char *arg = shift_args(argv, argc);

        if (*arg == '-') {
            const char *flag = arg+1;
            // TODO: We only support single char flags for now, eg: -h, -V
            if (*flag == '-') {
                log_error("We only support single char flags for now!");
                return 1;
            } else if (*flag == 'h') {
                help(program);
                return 0;
            } else if (*flag == 'v') {
                log_info("Compiler Version: %s", COMPILER_VERSION);
                return 0;
            } else {
                log_error("Invalid flag `%s`", flag);
                return 1;
            }

        } else {
            if (strcmp(arg, "help") == 0) {
                help(program);
                return 0;
            } else if (strcmp(arg, "dump_tokens") == 0) {
                dump_tokens = true;
            } else if (strcmp(arg, "dump_ast") == 0) {
                dump_ast = true;
            } else {
                if (filename == NULL) {
                    filename = arg;
                } else {
                    log_error("Invalid subcommand `%s`", arg);
                    return 1;
                }
            }
        }
    }

    if (filename == NULL) {
        error("Please provide a filename!");
        usage(program);
        return 1;
    }

    init();

    Lexer l = make_lexer(filename);

    if (l.src.count == 0) {
        log_error("That's an empty file, I don't know what to do with it...");
        return 1;
    }

    init_lexer_lines(&l);
    Tokens tokens = lex(&l);

    if (dump_tokens) {
        for (size_t i = 0; i < tokens.count; ++i) {
            print_token(stdout, tokens.items[i]);
            printf("\n");
        }
        return 0;
    }

    Parser p = make_parser(&l, tokens);

    Arena arena = arena_make(32*1024);

    AST_refs ast_refs = {0};

    AST *ast = parse(&arena, &p);
    while (ast != NULL) {
        if (!parser_match(&p, TK_SEMICOLON)) {
            error_pretty(parser_peek(&p).loc, (*p.lexer), "Expected semicolon but got '%s'", token_type_as_str(parser_peek(&p).type));
            return 1;
        }
        da_append(ast_refs, ast);
        if (parser_eof(&p)) {
            break;
        };
        ast = parse(&arena, &p);
    }

    if (dump_ast) {
        for (size_t i = 0; i < ast_refs.count; ++i) {
            AST *ast = ast_refs.items[i];
            print_ast(stdout, *ast); printf("\n");
        }
    }

    arena_free(&arena);
    free_parser(&p);
    free_lexer(&l);
    return 0;
}
