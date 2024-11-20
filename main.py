import sys
from enum import IntEnum, auto
import pprint

DEBUG = True

# Logging
def dlog(msg):
    if DEBUG:
        pprint.pp(f"[DEBUG] {msg}")

def info(msg):
    pprint.pp(f"[INFO] {msg}")

def error(msg):
    print(f"[ERROR] {msg}")

def fatal(msg, err_code = 1):
    error(msg)
    exit(err_code)

def usage(program: str):
    info(f"Usage: {program} <file>")

class Loc:
    def __init__(self, file, line, col):
        self.filename = file
        self.line = line
        self.col = col

    def __str__(self):
        return f"{self.filename}:{self.line}:{self.col}"

    def get_row(self, cur, bol):
        return cur - bol

class TokenType(IntEnum):
    IDENT = auto()
    STRING = auto()

    LEFT_PAREN = auto()
    RIGHT_PAREN = auto()
    MINUS = auto()
    RETURNER = auto()
    LEFT_BRACE = auto()
    RIGHT_BRACE = auto()
    PLUS = auto()
    DIVIDE = auto()
    MULTIPLY = auto()
    MODULUS = auto()
    EQUAL = auto()
    NOT = auto()
    NOT_EQUAL = auto()
    EQUAL_EQUAL = auto()
    GT = auto()
    LT = auto()
    GTE = auto()
    LTE = auto()
    COMMA = auto()
    COLON = auto()
    SEMICOLON = auto()
    DOT = auto()
    HASH = auto()
    LEFT_SQUARE_BRACE = auto()
    RIGHT_SQUARE_BRACE = auto()

    INT = auto()
    FLOAT = auto()
    COUNT = auto()

token_type_as_str_map: { TokenType : str } = {
    TokenType.IDENT                : "Ident",
    TokenType.STRING               : "String",
    TokenType.LEFT_PAREN           : "Left Paren",
    TokenType.RIGHT_PAREN          : "Right Paren",
    TokenType.MINUS                : "Minus",
    TokenType.LEFT_BRACE           : "Left Brace",
    TokenType.RIGHT_BRACE          : "Right Brace",
    TokenType.RETURNER             : "Returner",
    TokenType.PLUS                 : "Plus",
    TokenType.DIVIDE               : "Divide",
    TokenType.MULTIPLY             : "Multiply",
    TokenType.MODULUS              : "Modulus",
    TokenType.EQUAL                : "Equal",
    TokenType.NOT                  : "Not",
    TokenType.NOT_EQUAL            : "Not Equal",
    TokenType.EQUAL_EQUAL          : "Equal Equal",
    TokenType.GT                   : "Greater Than",
    TokenType.LT                   : "Less Than",
    TokenType.GTE                  : "Greater Than or Equal",
    TokenType.LTE                  : "Less Than or Equal",
    TokenType.COMMA                : "Comma",
    TokenType.COLON                : "Colon",
    TokenType.SEMICOLON            : "Semicolon",
    TokenType.DOT                  : "Dot",
    TokenType.HASH                 : "Hash",
    TokenType.LEFT_SQUARE_BRACE    : "Left Square Brace",
    TokenType.RIGHT_SQUARE_BRACE   : "Right Square Brace",
    TokenType.INT                  : "Int",
    TokenType.FLOAT                : "Float",
}
# NOTE: TokenType.COUNT - 1 because auto() starts from 1
assert len(token_type_as_str_map) == TokenType.COUNT-1, "Every TokenType is not handled in token_type_as_str_map"

class Token:
    def __init__(self, typ: TokenType, lexeme: str, loc: Loc):
        self.typ = typ
        self.lexeme = lexeme
        self.loc = loc

    def __str__(self):
        return f"Token ({token_type_as_str_map[self.typ]}, '{self.lexeme}', {self.loc})"

class Lexer:
    def __init__(self, filename: str):
        try:
            with open(filename, mode='r') as f:
                self.src = f.read()
        except FileNotFoundError:
            fatal(f"File '{filename}' not found!")
        self.cur = 0
        self.bol = 0
        self.line = 1
        self.filename = filename

    def row(self):
        return self.cur - self.bol

    def fatal(self, msg):
        error(f"{self.filename}:{self.line}:{self.row()}: {msg}")
        exit(1)

    def eof(self) -> bool:
        return self.cur >= len(self.src)

    def current_char(self) -> str:
        assert self.cur < len(self.src), f"cur: {self.cur}, src_len: {len(self.src)}"
        return self.src[self.cur]

    def peek_next_char(self) -> str | int:
        if self.cur + 1 >= len(self.src): return -1
        return self.src[self.cur + 1]

    def consume_char(self) -> str:
        c = self.current_char()
        self.cur += 1
        return c

    def consume_string(self) -> (str, Loc):
        assert self.current_char() == '"', "Called consume_string() at wrong character!"
        string: str = ''

        string_loc: Loc = Loc(self.filename, self.line, self.row())
        self.consume_char()
        c = self.consume_char()
        while c != '"':
            # dlog(f"String char '{c}'")
            if self.eof():
                self.fatal(f"Unterminated string!")
            string += c
            c = self.consume_char()

        # dlog(f"String: `{string}`")
        # info(f"String terminating char: {self.current_char()}")
        assert c == '"', "This shouldn't happen according to the while loop above"

        return (string, string_loc)

    def consume_identifier(self) -> (str, Loc):
        # Identifiers can start with [a-z][A-Z]_ and contain [0-9] after the first char
        assert self.current_char().isalpha() or self.current_char() == '_', "Called consume_identifier() at the wrong character!"
        ident: str = self.consume_char()
        ident_loc: Loc = Loc(self.filename, self.line, self.row())

        while (self.current_char().isalpha() or self.current_char() == '_' or self.current_char().isdigit()) and not self.eof():
            ident += self.consume_char()

        return (ident, ident_loc)

    def consume_number(self) -> (str, Loc):
        assert self.current_char().isdigit(), "Called consume_number() at the wrong character!"
        number: str = self.consume_char()

        # TODO: Handle numbers in other formats (Eg, binary, hexadecimal, etc)
        number_loc: Loc = Loc(self.filename, self.line, self.row())
        dot_handled: bool = False

        while (self.current_char().isdigit() or self.current_char() == '.') and not self.eof():
            if self.current_char() == '.':
                if dot_handled:
                    self.fatal("Number can only have one dots!")
                number += self.consume_char()
                dot_handled = True
            else:
                assert self.current_char().isdigit(), "This should never happen"
                number += self.consume_char()

        return (number, number_loc)

    def left_trim(self):
        while not self.eof() and self.current_char().isspace():
            if self.current_char() == '\n':
                self.line += 1
                self.bol = self.cur + 1
            self.consume_char()
            # dlog(f"Skipping {self.current_char()}")

        # dlog(f"Char after left trim: '{self.current_char()}'")

    def next_token(self) -> Token | None:
        self.left_trim()

        if self.eof():
            return None

        c = self.current_char()

        if c == '"':
            value, loc = self.consume_string()
            return Token(TokenType.STRING, value, loc)
        elif c.isalpha() or c == '_':
            ident, loc = self.consume_identifier()
            return Token(TokenType.IDENT, ident, loc)
        elif c == '(':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.LEFT_PAREN, self.consume_char(), loc)
        elif c == ')':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.RIGHT_PAREN, self.consume_char(), loc)
        elif c == '-':
            loc = Loc(self.filename, self.line, self.row())
            # Check if its a returner '->'
            if self.peek_next_char() == '>':
                return Token(TokenType.RETURNER, self.consume_char() + self.consume_char(), loc)

            return Token(TokenType.MINUS, self.consume_char(), loc)
        elif c == '{':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.LEFT_BRACE, self.consume_char(), loc)
        elif c == '}':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.RIGHT_BRACE, self.consume_char(), loc)
        elif c == '+':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.PLUS, self.consume_char(), loc)
        elif c == '/':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.DIVIDE, self.consume_char(), loc)
        elif c == '*':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.MULTIPLY, self.consume_char(), loc)
        elif c == '%':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.MODULUS, self.consume_char(), loc)
        elif c == '=':
            loc = Loc(self.filename, self.line, self.row())
            # Check if '=='
            if self.peek_next_char() == '=':
                return Token(TokenType.EQUAL_EQUAL, self.consume_char() + self.consume_char(), loc)
            return Token(TokenType.EQUAL, self.consume_char(), loc)
        elif c == '!':
            loc = Loc(self.filename, self.line, self.row())
            # Check if '!='
            if self.peek_next_char() == '=':
                return Token(TokenType.NOT_EQUAL, self.consume_char() + self.consume_char(), loc)
            return Token(TokenType.NOT, self.consume_char(), loc)
        elif c == '>':
            loc = Loc(self.filename, self.line, self.row())
            # Check if '>='
            if self.peek_next_char() == '=':
                return Token(TokenType.GTE, self.consume_char() + self.consume_char(), loc)
            return Token(TokenType.GT, self.consume_char(), loc)
        elif c == '<':
            loc = Loc(self.filename, self.line, self.row())
            # Check if '<='
            if self.peek_next_char() == '=':
                return Token(TokenType.LTE, self.consume_char() + self.consume_char(), loc)
            return Token(TokenType.LT, self.consume_char(), loc)
        elif c == ',':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.COMMA, self.consume_char(), loc)
        elif c == ':':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.COLON, self.consume_char(), loc)
        elif c == ';':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.SEMICOLON, self.consume_char(), loc)
        elif c == '[':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.LEFT_SQUARE_BRACE, self.consume_char(), loc)
        elif c == ']':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.RIGHT_SQUARE_BRACE, self.consume_char(), loc)
        elif c == '.':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.DOT, self.consume_char(), loc)
        elif c == '#':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.HASH, self.consume_char(), loc)
        elif c.isdigit():
            num, loc = self.consume_number()
            return Token(TokenType.FLOAT if num.find(".") != -1 else TokenType.INT, num, loc)
        else:
            fatal(f"Unrecognized character '{c}'")

        return None

    def lex(self) -> [Token]:
        tokens: [Token] = []
        token = self.next_token()
        while token != None:
            tokens.append(token)
            token = self.next_token()
        return tokens


class AstNodeType(IntEnum):
    EXPR  = auto()
    STMT  = auto()
    INT   = auto()
    FLOAT = auto()
    IDENT = auto()
    COUNT = auto()

ast_node_type_as_str_map: {AstNodeType : str} = {
    AstNodeType.EXPR   : "Expr",
    AstNodeType.STMT   : "Stmt",
    AstNodeType.INT    : "Int",
    AstNodeType.FLOAT  : "Float",
    AstNodeType.IDENT  : "Identifier",
}

assert len(ast_node_type_as_str_map) == AstNodeType.COUNT-1, "Every AstNodeType is not handled in ast_node_type_as_str_map"

'''
NOTE: ¢ is a zero length string, meaning nothing will be substituted
Grammar:
    Statement       => Ident :? Ident? =? Expression* ;
    Expression      => Name Binop Name
    Name            => LitValue | Ident
    LitValue        => Int | Float | String
    BinaryOperator  => ArithmeticOp | ComparisionOp | LogicalOp
    ComparisionOp   => > | >= | < | <= | == | !=
    ArithmeticOp    => + | - | * | / | %
    LogicalOp       => && | '||'
    BinArithmeticOp => ^ | '|' | &
'''

class AstNode:
    def __init__(self, token: Token, typ: AstNodeType):
        self.token = token
        self.typ = typ

class AstNodeStatement(AstNode):
    def __init__(self, token: Token):
        super().__init__(token, AstNodeType.STMT)

class AstNodeExpression(AstNode):
    def __init__(self, token: Token):
        super().__init__(token, AstNodeType.EXPR)

class AstNodeInt(AstNode):
    def __init__(self, token: Token, value: int):
        super().__init__(token, AstNodeType.INT)
        self.value = value

class AstNodeIdentifier(AstNode):
    def __init__(self, token: Token, name: str):
        super().__init__(token, AstNodeType.IDENT)
        self.name = name

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens

    def syntax_error(self, msg: str, token: Token):
        fatal(f"{str(token.loc)}: Syntax Error: {msg}")

    def parse(self) -> AstNode:
        stmt = self.parseStatement()
        return stmt

    def parseStatement(self) -> AstNodeStatement:
        tokens = self.tokens
        # Variable name
        ident_ast = self.parseIdentifier()
        var_typ  = None
        # Check if colon is there
        if len(tokens) >= 1 and tokens[0].typ == TokenType.COLON:
            # TODO: Should i make an AstNode for the colon too?
            colon = tokens.pop(0)
            var_typ = None if len(tokens) <= 0 else tokens.pop(0)
            if var_typ == None or var_typ.typ != TokenType.IDENT:
                if var_typ == None: error_msg = "Reached End of File"
                else: error_msg = f"Got {token_type_as_str_map[var_typ.typ]}"
                self.syntax_error("Expected type of variable after colon, but %s" % error_msg, colon)

        dlog(ident_ast.token)
        if var_typ != None:
            dlog(var_typ)

        print("TODO: Implement parseStatement()")
        exit(1)
        # expr = self.parseExpr()

    def parseIdentifier(self) -> AstNodeIdentifier:
        if len(self.tokens) <= 0: return None
        if self.tokens[0].typ != TokenType.IDENT: return None
        ident_token = self.tokens.pop(0)
        return AstNodeIdentifier(ident_token, ident_token.lexeme)

    def parseExpr(self) -> AstNodeExpression:
        pass

def main():
    program: str = sys.argv.pop(0)

    if len(sys.argv) <= 0:
        error(f"Please provide a filename!")
        usage(program)
        exit(1)

    filename: str = sys.argv.pop(0)

    lexer = Lexer(filename)
    # Lexical Analysis
    tokens = lexer.lex()

    # TODO: Parse
    parser = Parser(tokens)

    # for t in tokens:
    #     pprint.pp(str(t))

    parser.parse()

if __name__ == '__main__':
    main()
