#include <stdio.h>
#include <ctype.h>

#define COMMONLIB_IMPLEMENTATION
#define COMMONLIB_REMOVE_PREFIX
#include <commonlib.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define error log_error
#define info log_info

#define COMPILER_VERSION "v0.0.6"

static bool DEBUG_PRINT = false;

// TODO:Implement every expression parsing for C:
// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// suffixes       → ( "++" | "--" ) IDENT
// primary        → NUMBER | STRING | IDENT | "true" | "false" | "null"
//                | "(" expression ")" ;

/* NOTE: We are referencing this table: https://en.cppreference.com/w/c/language/operator_precedence
 * PRECEDENCE TABLE
 *
 * LOW
 *  |
 *  v
 * HIGH
 *
 * NAME                | OP                                | ASSOCIATE
 * --------------------+-----------------------------------+-----------
 * Comma               | ,                                 | Left
 * --------------------+-----------------------------------+-----------
 * Bitwise Assignment  | &= |= ^=                          | Right
 * --------------------+-----------------------------------+-----------
 * Bitshift Assignment | <<= >>=                           | Right
 * --------------------+-----------------------------------+-----------
 * Factor Assignment   | /= *= %=                          | Right
 * --------------------+-----------------------------------+-----------
 * Term Assignment     | += -=                             | Right
 * --------------------+-----------------------------------+-----------
 * Simple Assignment   | =                                 | Right
 * --------------------+-----------------------------------+-----------
 * Ternary Condition   | ?:                                | Right
 * --------------------+-----------------------------------+-----------
 * Logical OR          | ||                                | Left
 * --------------------+-----------------------------------+-----------
 * Logical AND         | &&                                | Left
 * --------------------+-----------------------------------+-----------
 * Bitwise OR          | |                                 | Left
 * --------------------+-----------------------------------+-----------
 * Bitwise XOR         | ^                                 | Left
 * --------------------+-----------------------------------+-----------
 * Bitwise AND         | &                                 | Left
 * --------------------+-----------------------------------+-----------
 * Equality            | == !=                             | Left
 * --------------------+-----------------------------------+-----------
 * Comparision         | > >= < <=                         | Left
 * --------------------+-----------------------------------+-----------
 * Bit shift           | << >>                             | Left
 * --------------------+-----------------------------------+-----------
 * Term                | - +                               | Left
 * --------------------+-----------------------------------+-----------
 * Factor              | / * %                             | Left
 * --------------------+-----------------------------------+-----------
 * sizeof              | sizeof                            | Right
 * --------------------+-----------------------------------+-----------
 * Address-of          | &                                 | Right
 * --------------------+-----------------------------------+-----------
 * Dereference         | *                                 | Right
 * --------------------+-----------------------------------+-----------
 * Cast                | (type)                            | Right
 * --------------------+-----------------------------------+-----------
 * L/B NOT             | ! ~                               | Right
 * --------------------+-----------------------------------+-----------
 * Unary Plus/Minus    | + -                               | Right
 * --------------------+-----------------------------------+-----------
 * Prefix Inc/Dec      | ++ --                             | Right
 * --------------------+-----------------------------------+-----------
 * Compound Lit        | (type){list}                      | Left
 * --------------------+-----------------------------------+-----------
 * Struct/Union access | .                                 | Left      NOTE: We use . to access through pointers as well
 * --------------------+-----------------------------------+-----------
 * Array Subscripting  | []                                | Left
 * --------------------+-----------------------------------+-----------
 * Function Call       | ()                                | Left
 * --------------------+-----------------------------------+-----------
 * Suffix Inc/Dec      | ++ --                             | Left
 * --------------------+-----------------------------------+-----------
 * Primary             | IDENTS NUMBERS STRINGS CHARS (expr) | -
 * --------------------+-----------------------------------+-----------
 */


/// NOTE: Location
typedef struct {
    const char *filename;
    int line;
    int col;
} Location;

void print_loc(FILE *f, Location loc);

///

/// NOTE: Token

typedef enum {
    TK_IDENT,
    TK_KEYWORD,

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
} Token;

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
    size_t offset;
    size_t count;
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
    size_t line;
    Lines lines;
    const char *filename;
} Lexer;

Lexer make_lexer(const char *filename);
Tokens lex(Lexer *l);
bool next_token(Lexer *l, Token *t_out);
String_view get_src_copy(Lexer *l);
bool eof(Lexer *l);
int col(Lexer *l);
char current_char(Lexer *l);
char next_char(Lexer *l);
char consume_char(Lexer *l);
void consume_ident(Lexer *l, String_view *ident_sv_out, Location *loc_out);
void consume_string(Lexer *l, String_view *string_sv_out, Location *loc_out);
void consume_character(Lexer *l, String_view *char_sv_out, Location *loc_out);
void consume_single_char(Lexer *l, String_view *sv_out, Location *loc_out);
void consume_number(Lexer *l, String_view *sv_out, Location *loc_out);
void consume_comment(Lexer *l, String_view *sv_out, Location *loc_out);
void left_trim(Lexer *l);
bool next_token(Lexer *l, Token *t_out);
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
Token parser_peek(Parser *p);
bool parser_eof(Parser *p);
void free_parser(Parser *p);

///

/// NOTE: Expressions
typedef struct Expression Expression;
typedef struct Unary_expression Unary_expression;
typedef struct Binary_expression Binary_expression;
typedef struct Primary_expression Primary_expression;
typedef struct Literal Literal;
typedef enum   Literal_kind Literal_kind;
typedef enum   Primary_expression_kind Primary_expression_kind;

struct Literal {
    union {
        float f;
        int i;
        bool b;
        char ch;
        char *str;
    } as;
};

void print_literal(FILE *f, Literal value, Literal_kind kind);

enum Literal_kind {
    LIT_FLOAT,
    LIT_INT,
    LIT_BOOL,
    LIT_CHAR,
    LIT_STRING,
    LIT_COUNT,
};

const char *lit_kind_as_str(Literal_kind k);

struct Unary_expression {
    Token operator;
    Expression *operand;
};

struct Binary_expression {
    Token operator;
    Expression *lhs;
    Expression *rhs;
};

enum Primary_expression_kind {
    PRIMARY_VALUE,
    PRIMARY_IDENT,
    PRIMARY_COUNT,
};

struct Primary_expression {
    Primary_expression_kind kind;
    Literal value;
    Literal_kind value_kind;
    String_view identifier_key;
};

void print_primary_expression(FILE *f, Primary_expression *pe);

typedef enum {
    EXPR_BINARY,
    EXPR_UNARY,
    EXPR_PRIMARY,
    EXPR_COUNT,
} Expression_kind;

const char *expression_kind_as_str(Expression_kind k);

struct Expression {
    Expression_kind kind;
    Binary_expression *bin_expr;
    Primary_expression *prim_expr;
    Unary_expression *unary_expr;
    Location loc;
};

void print_expression_as_value(FILE *f, Expression e);
void print_expression(FILE *f, Expression e);

typedef struct {
    Expression **items;
    size_t count;
    size_t capacity;
} Expression_refs;

///

/// Identifiers

typedef struct {
    String_view name;
    Literal value;
    Literal_kind value_kind;
    bool not_declared;
    Primary_expression *prim_expr;
} Identifier;

typedef struct {
    String_view key;
    Identifier value;
} Identifier_KV;

static Identifier_KV *identifier_map = {0};

///

void usage(const char *program) {
    info("Usage: %s [subcommand] [flag(s)] <file>", program);
}

void help(const char *program) {
    usage(program);

    info("");
    info("Flags: ");
    info("  -h           Prints this help message.");
    info("  -v           Prints the version of the compiler.");
    info("Subcommands: ");
    info("  help         Prints this help message.");
    info("  dump_tokens  Dumps the tokens scanned and exit.");
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
        String_view line_sv = sv_get_part((lexer).src, line.offset, line.offset + line.count);\
        error(fmt, ##__VA_ARGS__);\
        printf(SV_FMT"\n", SV_ARG(line_sv));\
        printf("%*s^\n", (loc).col, "");\
    } while (0)

bool token_is_number(Token t) {
    return t.type == TK_INT || t.type == TK_FLOAT;
}

void print_literal(FILE *f, Literal value, Literal_kind kind) {
    switch (kind) {
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

const char *expression_kind_as_str(Expression_kind k) {
    switch (k) {
        case EXPR_BINARY: return "BINARY";
        case EXPR_UNARY: return "UNARY";
        case EXPR_PRIMARY: return "PRIMARY";
        case EXPR_COUNT:
        default: ASSERT(false, "UNREACHABLE!");
    }

    return "YOU SHOULD NOT SEE THIS!";
}

void print_primary_expression(FILE *f, Primary_expression *pe) {
    if (pe->kind == PRIMARY_VALUE) {
        print_literal(f, pe->value, pe->value_kind);
    } else if (pe->kind == PRIMARY_IDENT) {
        Identifier_KV *ident_kv = hmgetp_null(identifier_map, pe->identifier_key);
        ASSERT(ident_kv != NULL, "The identifier should be in the identifier_map!");
        Identifier ident = ident_kv->value;

        fprintf(f, "[IDENT] '"SV_FMT"': ", SV_ARG(ident.name));
        if (ident.not_declared) {
            fprintf(f, "<NOTSET>");
        } else {
            print_literal(f, ident.value, ident.value_kind);
        }
    } else {
        ASSERT(false, "UNREACHABLE!");
    }
}

void print_expression_as_value(FILE *f, Expression e) {
    switch (e.kind) {
        case EXPR_BINARY: {
            fprintf(f, "(");
            print_expression_as_value(f, *e.bin_expr->lhs);
            fprintf(f, " %s ", token_type_as_str(e.bin_expr->operator.type));
            print_expression_as_value(f, *e.bin_expr->rhs);
            fprintf(f, ")");

        } break;
        case EXPR_UNARY: {
            fprintf(f, " %s ", token_type_as_str(e.unary_expr->operator.type));
            fprintf(f, "(");
            print_expression_as_value(f, *e.unary_expr->operand);
            fprintf(f, ")");
        } break;
        case EXPR_PRIMARY: {
            print_primary_expression(f, e.prim_expr);
        } break;
        case EXPR_COUNT:
        default: ASSERT(false, "UNREACHABLE!");
    }
}

void print_expression(FILE *f, Expression e) {
    print_loc(f, e.loc);
    fprintf(f, " [%s] '", expression_kind_as_str(e.kind));
    print_expression_as_value(f, e);
    fprintf(f, "'");
}


const char *token_type_as_str(Token_type t) {
    switch (t) {
        case TK_IDENT: return "IDENT";
        case TK_KEYWORD: return "KEYWORD";
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

Token parser_previous(Parser *p) {
    ASSERT(0 <= p->current_token_id-1 && (size_t)p->current_token_id-1 <= p->tokens.count-1, "OutofBounds");
    return p->tokens.items[p->current_token_id-1];
}

Token parser_peek(Parser *p) {
    ASSERT(0 <= p->current_token_id && (size_t)p->current_token_id <= p->tokens.count-1, "OutofBounds");
    return p->tokens.items[p->current_token_id];
}

bool parser_eof(Parser *p) {
    return parser_peek(p).type == TK_EOF;
}

// Token parser_current_token(Parser *p) {
//     ASSERT(0 <= p->current_token_id && (size_t)p->current_token_id <= p->tokens.count - 1, "Parser.current_token_id outofbounds!");
//     return p->tokens.items[p->current_token_id];
// }


// Predecls
Expression *parse_primary(Arena *arena, Parser *p);
Expression *parse_suffixes(Arena *arena, Parser *p);
Expression *parse_unary(Arena *arena, Parser *p);
Expression *parse_factor(Arena *arena, Parser *p);
Expression *parse_comparision(Arena *arena, Parser *p);
Expression *parse_term(Arena *arena, Parser *p);
Expression *parse_equality(Arena *arena, Parser *p);
Expression *parse_expression(Arena *arena, Parser *p);

Expression *parse_primary(Arena *arena, Parser *p) {
    // NOTE: We can advance here because primary is the last rule
    // TODO: Somehow parser_advance() here breaks it.
    Token t = parser_peek(p);

    if (t.type != TK_LEFT_PAREN) {
        Expression *expr = (Expression *)arena_alloc(arena, sizeof(Expression));
        expr->kind = EXPR_PRIMARY;
        expr->loc = t.loc;
        expr->prim_expr = (Primary_expression *)arena_alloc(arena, sizeof(Primary_expression));
        expr->prim_expr->kind = PRIMARY_VALUE;
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

            expr->prim_expr->identifier_key = t.lexeme;
            if (ident.not_declared) {
                // If the ident is not declared yet, mark the primary_expr it needs to update the value of for later.
                ident_kv->value.prim_expr = expr->prim_expr;
            } else {
                // If the ident is declared, set the value of the expression!
                expr->prim_expr->value = ident.value;
                expr->prim_expr->value_kind = ident.value_kind;
            }
            expr->prim_expr->kind = PRIMARY_IDENT;

            return expr;
            ASSERT(false, "UNIMPLEMENTED!");
        } else if (token_is_number(t)) {
            if (t.type == TK_INT) {
                int i_count = -1;
                int i = sv_to_int(t.lexeme, &i_count, 10);
                ASSERT(i_count != -1, "We made a mistake in lexing of integers!");
                expr->prim_expr->value.as.i = i;
                expr->prim_expr->value_kind = LIT_INT;
                return expr;
            } else if (t.type == TK_FLOAT) {
                int f_count = -1;
                float f = sv_to_float(t.lexeme, &f_count);
                ASSERT(f_count != -1, "We made a mistake in lexing of floats!");
                expr->prim_expr->value.as.f = f;
                expr->prim_expr->value_kind = LIT_FLOAT;
                return expr;
            } else {

            }
        } else if (t.type == TK_STRING) {
            expr->prim_expr->value_kind = LIT_STRING;
            expr->prim_expr->value.as.str = sv_to_cstr(t.lexeme);
            return expr;
        } else if (t.type == TK_CHAR) {
            expr->prim_expr->value_kind = LIT_CHAR;
            expr->prim_expr->value.as.ch = *t.lexeme.data;
            return expr;
        } else if (t.type == TK_BOOL) {
            expr->prim_expr->value_kind = LIT_BOOL;
            expr->prim_expr->value.as.b = sv_equals(t.lexeme, SV("true"));
            return expr;
        }
    } else {
        parser_advance(p); // Skip (
        Expression *expr = parse_expression(arena, p);
        if (parser_peek(p).type != TK_RIGHT_PAREN) {
            Token t = parser_peek(p);
            error_pretty(t.loc, (*p->lexer), "Expected ), But got `%s`", token_type_as_str(t.type));
            return NULL;
        }
        parser_advance(p); // Skip )
        return expr;
    }

    error_pretty(t.loc, (*p->lexer), "Expected expression, but got `%s`", token_type_as_str(t.type));
    return NULL;
}

Expression *parse_suffixes(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_PLUS_PLUS || t.type == TK_MINUS_MINUS) {
        parser_advance(p); // Skip ++ | --
        // TODO: It could be (IDENT) too?
        if (parser_peek(p).type != TK_IDENT) {
            /*error_pretty(parser_peek(p).loc, (*p->lexer), "Expected Identifier But got `%s`", token_type_as_str(parser_peek(p).type));*/
            Token unwanted_token = parser_peek(p);
            error_pretty(unwanted_token.loc, (*p->lexer),
                         "You cannot pre-%s `%s`! Only identifiers!",
                         (t.type == TK_PLUS_PLUS ? "increment" : "decrement"),
                         token_type_as_str(unwanted_token.type));
            return NULL;
        }
        Expression *expr = (Expression *)arena_alloc(arena, sizeof(Expression));
        expr->loc = t.loc;
        expr->unary_expr = (Unary_expression *)arena_alloc(arena, sizeof(Unary_expression));
        expr->kind = EXPR_UNARY;
        Unary_expression *unary_expr = expr->unary_expr;
        unary_expr->operator = parser_previous(p);
        unary_expr->operand = parse_primary(arena, p);
        ASSERT(unary_expr->operand != NULL, "We know that the next expr is an identifier!");
        return expr;
    }
    return parse_primary(arena, p);
}

Expression *parse_unary(Arena *arena, Parser *p) {
    Token t = parser_peek(p);

    if (t.type == TK_NOT || t.type == TK_MINUS) {
        Expression *expr = (Expression *)arena_alloc(arena, sizeof(Expression));
        expr->loc = t.loc;
        expr->unary_expr = (Unary_expression *)arena_alloc(arena, sizeof(Unary_expression));
        expr->kind = EXPR_UNARY;
        Unary_expression *unary_expr = expr->unary_expr;
        unary_expr->operator = parser_advance(p);
        unary_expr->operand = parse_unary(arena, p);
        return expr;
    }

    return parse_suffixes(arena, p);
}

Expression *parse_factor(Arena *arena, Parser *p) {
    Expression *expr = parse_unary(arena, p);
    if (expr == NULL) return NULL;

    while (parser_match(p, TK_DIVIDE) || parser_match(p, TK_MULTIPLY)) {
        Token op = parser_previous(p);
        Expression *rhs = parse_unary(arena, p);
        if (rhs == NULL) return rhs;

        Expression *new_expr = (Expression *)arena_alloc(arena, sizeof(Expression));
        new_expr->kind = EXPR_BINARY;
        new_expr->loc = expr->loc;
        new_expr->bin_expr = (Binary_expression *)arena_alloc(arena, sizeof(Binary_expression));
        new_expr->bin_expr->lhs = expr;
        new_expr->bin_expr->operator = op;
        new_expr->bin_expr->rhs = rhs;

        expr = new_expr;
    }

    return expr;
}

Expression *parse_term(Arena *arena, Parser *p) {
    Expression *expr = parse_factor(arena, p);
    if (expr == NULL) return NULL;
    /*printf("factor expr: %p\n", expr);*/

    while (parser_match(p, TK_MINUS) || parser_match(p, TK_PLUS)) {
        Token operator = parser_previous(p);

        Expression *rhs = parse_factor(arena, p);
        if (rhs == NULL) return NULL;

        Expression *new_expr = (Expression *)arena_alloc(arena, sizeof(Expression));
        new_expr->kind = EXPR_BINARY;
        new_expr->loc = expr->loc;
        new_expr->bin_expr = (Binary_expression *)arena_alloc(arena, sizeof(Binary_expression));
        new_expr->bin_expr->lhs = expr;
        new_expr->bin_expr->operator = operator;
        new_expr->bin_expr->rhs = rhs;

        expr = new_expr;
    }

    return expr;
}

Expression *parse_comparision(Arena *arena, Parser *p) {
    Expression *expr = parse_term(arena, p);
    if (expr == NULL) return NULL;
    /*printf("term expr: %p\n", expr);*/

    while (parser_match(p, TK_GT) || parser_match(p, TK_GTE) ||
           parser_match(p, TK_LT) || parser_match(p, TK_LTE)) {
        Token operator = parser_previous(p);

        Expression *rhs = parse_term(arena, p);
        if (rhs == NULL) return NULL;

        Expression *new_expr = (Expression *)arena_alloc(arena, sizeof(Expression));
        new_expr->kind = EXPR_BINARY;
        new_expr->loc = expr->loc;
        new_expr->bin_expr = (Binary_expression *)arena_alloc(arena, sizeof(Binary_expression));
        new_expr->bin_expr->lhs = expr;
        new_expr->bin_expr->operator = operator;
        new_expr->bin_expr->rhs = rhs;

        expr = new_expr;
    }

    return expr;
}

Expression *parse_equality(Arena *arena, Parser *p) {
    Expression *expr = parse_comparision(arena, p);
    if (expr == NULL) return NULL;
    /*printf("comparision expr: %p\n", expr);*/

    while (parser_match(p, TK_NOT_EQUAL) || parser_match(p, TK_EQUAL_EQUAL)) {
        Token operator = parser_previous(p);

        Expression *rhs = parse_comparision(arena, p);
        if (rhs == NULL) return NULL;

        Expression *new_expr = (Expression *)arena_alloc(arena, sizeof(Expression));
        new_expr->kind = EXPR_BINARY;
        new_expr->loc = expr->loc;
        new_expr->bin_expr = (Binary_expression *)arena_alloc(arena, sizeof(Binary_expression));
        new_expr->bin_expr->lhs = expr;
        new_expr->bin_expr->operator = operator;
        new_expr->bin_expr->rhs = rhs;

        expr = new_expr;
    }

    return expr;
}

Expression *parse_expression(Arena *arena, Parser *p) {
    Expression *expr = parse_equality(arena, p);
    /*printf("equality expr: %p\n", expr);*/
    return expr;
}

Lexer make_lexer(const char *filename) {
    int file_size = -1;
    const char *buf = read_file(filename, &file_size);
    if (file_size == -1) {
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
        error_pretty((*loc_out), (*l), "Unterminated string!"); 
        exit(1);
    }

    // Eat "
    consume_char(l);
}

void consume_character(Lexer *l, String_view *char_sv_out, Location *loc_out) {
    ASSERT(current_char(l) == '\'', "We except '\'' to be the current_char here...");

    // Eat '
    consume_char(l);

    String_view src_copy = get_src_copy(l);

    loc_out->filename = l->filename;
    loc_out->line     = l->line;
    loc_out->col      = col(l);

    *char_sv_out = sv_lpop(&src_copy, 1);

    l->cur += 1;

    if (current_char(l) != '\'') {
        error_pretty(*loc_out, *l, "Expected `'`, but got `%ch`", current_char(l));
        exit(1);
    }
    if (eof(l)) {
        error_pretty(*loc_out, *l, "Unterminated char!"); 
        exit(1);
    }

    // Eat '
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
            error_pretty(loc, *l, "Unterminated comment!");
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
            Line line = {
                .offset = l->bol,
                .count = col(l),
            };
            da_append(l->lines, line);
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
        case '\'': {
            String_view char_sv = {0};
            Location char_loc = {0};
            consume_character(l, &char_sv, &char_loc);
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
    Line last_line = l->lines.items[l->lines.count-1];
    t.lexeme = SV("EOF");
    t.loc.filename = l->filename;
    t.loc.line = l->line-1;
    t.loc.col = last_line.count;
    t.type = TK_EOF;
    da_append(tokens, t);

    return tokens;
}

typedef struct {
    const char **items;
    size_t count;
    size_t capacity;
} Flags;

int main(int argc, char **argv) {
    const char *program = shift_args(argv, argc);

    const char *filename = NULL;

    bool dump_tokens = false;

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

    Lexer l = make_lexer(filename);

    Tokens tokens = lex(&l);

    if (dump_tokens) {
        for (size_t i = 0; i < tokens.count; ++i) {
            print_token(stdout, tokens.items[i]);
            printf("\n");
        }
        return 0;
    }

    Parser p = make_parser(&l, tokens);

    Arena expr_arena = arena_make(0);

    Expression_refs expr_refs = {0};

    Expression *expr = parse_expression(&expr_arena, &p);
    while (expr != NULL) {
        if (!parser_match(&p, TK_SEMICOLON)) {
            error_pretty(parser_peek(&p).loc, (*p.lexer), "Expected semicolon but got '%s'", token_type_as_str(parser_peek(&p).type));
            return 1;
        }
        da_append(expr_refs, expr);
        if (parser_eof(&p)) {
            break;
        };
        expr = parse_expression(&expr_arena, &p);
    }

    for (size_t i = 0; i < expr_refs.count; ++i) {
        Expression *expr = expr_refs.items[i];
        print_expression(stdout, *expr); printf("\n");
    }

    arena_free(&expr_arena);
    free_parser(&p);
    free_lexer(&l);
    return 0;
}
