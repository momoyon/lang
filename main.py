import sys
from enum import IntEnum, auto
import pprint

from typing import Tuple, List, cast

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

    BINARY_AND = auto()
    BINARY_NOT = auto()
    BINARY_OR = auto()
    LOGICAL_AND = auto()
    LOGICAL_OR = auto()

    COUNT = auto()

token_type_as_str_map = {
    TokenType.IDENT                : "Identifier",
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
    TokenType.BINARY_AND           : "Binary And",
    TokenType.BINARY_OR            : "Binary Or",
    TokenType.BINARY_NOT           : "Binary Not",
    TokenType.LOGICAL_AND          : "Logical And",
    TokenType.LOGICAL_OR           : "Logical Or",
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

    def consume_string(self) -> Tuple[str, Loc]:
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

    def consume_identifier(self) -> Tuple[str, Loc]:
        # Identifiers can start with [a-z][A-Z]_ and contain [0-9] after the first char
        assert self.current_char().isalpha() or self.current_char() == '_', "Called consume_identifier() at the wrong character!"
        ident: str = self.consume_char()
        ident_loc: Loc = Loc(self.filename, self.line, self.row())

        while (self.current_char().isalpha() or self.current_char() == '_' or self.current_char().isdigit()) and not self.eof():
            ident += self.consume_char()

        return (ident, ident_loc)

    def consume_number(self) -> Tuple[str, Loc]:
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
        elif c == '&':
            loc = Loc(self.filename, self.line, self.row())
            # Check if '&&'
            if self.peek_next_char() == '&':
                return Token(TokenType.LOGICAL_AND, self.consume_char() + self.consume_char(), loc)
            return Token(TokenType.BINARY_AND, self.consume_char(), loc)
        elif c == '|':
            loc = Loc(self.filename, self.line, self.row())
            # Check if '||'
            if self.peek_next_char() == '|':
                return Token(TokenType.LOGICAL_OR, self.consume_char() + self.consume_char(), loc)
            return Token(TokenType.BINARY_OR, self.consume_char(), loc)
        elif c == '~':
            loc = Loc(self.filename, self.line, self.row())
            return Token(TokenType.BINARY_NOT, self.consume_char(), loc)
        else:
            fatal(f"Unrecognized character '{c}'")

        return None

    def lex(self) -> List[Token]:
        tokens: List[Token] = []
        token: Token | None = self.next_token()
        while token != None:
            tokens.append(cast(Token, token))
            token = self.next_token()
        return tokens


class AstNodeType(IntEnum):
    EXPR  = auto()
    STMT  = auto()
    INT   = auto()
    FLOAT = auto()
    IDENT = auto()
    STRING = auto()
    BINARY_OP = auto()
    COLON = auto()
    COUNT = auto()

ast_node_type_as_str_map: dict[AstNodeType, str] = {
    AstNodeType.EXPR      : "Expr",
    AstNodeType.STMT      : "Stmt",
    AstNodeType.INT       : "Int",
    AstNodeType.FLOAT     : "Float",
    AstNodeType.IDENT     : "Identifier",
    AstNodeType.STRING    : "String",
    AstNodeType.BINARY_OP : "Binary Op",
    AstNodeType.COLON     : "Colon",
}

assert len(ast_node_type_as_str_map) == AstNodeType.COUNT-1, "Every AstNodeType is not handled in ast_node_type_as_str_map"

'''
NOTE: ¢ is a zero length string, meaning nothing will be substituted
Grammar:
    Statement       => Ident :? Ident? =? Expression* ';' ;
    Expression      => Name (Binop Name)* ;
    Name            => LitValue | Ident ;
    LitValue        => Int | Float | String ;
    BinaryOperator  => ArithmeticOp | ComparisonOp | LogicalOp | BinArithmeticOp ;
    ComparisonOp    => > | >= | < | <= | == | != ;
    ArithmeticOp    => + | - | * | / | % ;
    LogicalOp       => && | '||' ;
    BinArithmeticOp => ^ | '|' | & ;
'''

class AstNode:
    def __init__(self, token: Token, typ: AstNodeType):
        self.token = token
        self.typ = typ

class AstNodeIdentifier(AstNode):
    def __init__(self, token: Token, name: str):
        super().__init__(token, AstNodeType.IDENT)
        self.name = name

    def __repr__(self):
        return f"IDENT: {self.name}"

class AstNodeColon(AstNode):
    def __init__(self, token: Token, ident: AstNodeIdentifier, typ: AstNodeIdentifier):
        super().__init__(token, AstNodeType.COLON)
        self.ident = ident
        self.var_type = typ

    def __repr__(self):
        return f"COLON: {self.var_type}"

class AstNodeExpression(AstNode):
    def __init__(self, token: Token, lhs, binop, rhs):
        super().__init__(token, AstNodeType.EXPR)
        self.lhs = lhs
        self.binop = binop
        self.rhs = rhs

    def __repr__(self):
        return f"EXPR: {self.lhs.__repr__()} {self.binop.__repr__()} {self.rhs.__repr__()}"

class AstNodeStatement(AstNode):
    def __init__(self, token: Token, var_name: AstNodeIdentifier, colon_ast: AstNodeColon | None, expr: AstNodeExpression | None):
        super().__init__(token, AstNodeType.STMT)
        self.var_name: AstNodeIdentifier = var_name
        self.colon: AstNodeColon | None = colon_ast
        self.expr: AstNodeExpression | None = expr

    def __repr__(self):
        if self.colon == None:
            return f"STMT: {self.var_name}"
        return f"STMT: {self.var_name.__repr__()} : {self.colon} = {self.expr.__repr__()}"

class AstNodeInt(AstNode):
    def __init__(self, token: Token, value: int):
        super().__init__(token, AstNodeType.INT)
        self.value = value

    def __repr__(self):
        return f"INT: {self.value}"

class AstNodeFloat(AstNode):
    def __init__(self, token: Token, value: float):
        super().__init__(token, AstNodeType.FLOAT)
        self.value = value

    def __repr__(self):
        return f"FLOAT: {self.value}"

class AstNodeString(AstNode):
    def __init__(self, token: Token, string: str):
        super().__init__(token, AstNodeType.STRING)
        self.string = string

    def __repr__(self):
        return f"STRING: '{self.string}'"

class AstNodeBinaryOp(AstNode):
    def __init__(self, token: Token):
        super().__init__(token, AstNodeType.BINARY_OP)
        self.op = self.token.lexeme

    def __repr__(self):
        return f"BINOP: {self.op}"

# class ParseError(IntEnum):
#     EOF = auto()
#     UNEXPECTED_TOKEN = auto()
#     NAH = auto()
#     COUNT = auto()

# parse_error_as_str_map: dict[ParseError, str] = {
#     "Reached End of File!" : "Eof",
#     ParseError.UNEXPECTED_TOKEN : "Unexpected Token",
#     ParseError.NAH : "Nah",
# }
# assert len(parse_error_as_str_map) == ParseError.COUNT-1, "Every ParseError is not handled in parse_error_as_str_map"

class ParseUnexpectedType(Exception):
    def __init__(self, got: Token, *expected_types: list[TokenType]):
        self.expected_types: list[TokenType] = expected_types
        self.got: Token = got

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        buf: str = "Parse Error: Expected "
        for i in range(len(self.expected_types)):
            e: TokenType = self.expected_types[i]
            buf += f"{token_type_as_str_map[e]}"
            if i != len(self.expected_types)-1:
                buf += " or "
        buf += f", But got {token_type_as_str_map[self.got.typ]}"
        return buf

class ParseEOF(Exception):
    def __init__(self, *expected_types: list[TokenType]):
        self.expected_types: list[TokenType] = expected_types

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        buf: str = "Parse Error: Expected "
        for i in range(len(self.expected_types)):
            e: TokenType = self.expected_types[i]
            buf += f"{token_type_as_str_map[e]}"
            if i != len(self.expected_types)-1:
                buf += " or "
        buf += ", But reached EOF"

        return buf

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens

    def syntax_error(self, msg: str, token: Token):
        fatal(f"{str(token.loc)}: Syntax Error: {msg}")

    def parse(self) -> AstNode:
        stmt = None
        try:
            stmt = self.parseStatement()
        except Exception as e:
            error(e)
            exit(1)
        return stmt

    def parseStatement(self) -> AstNodeStatement:
        tokens = self.tokens

        # Variable name
        var_name_ast = self.parseIdentifier()
        var_type_ast = None

        # WIP: Check if this statement is an assignment or just {ident;}
        colon_ast: AstNodeColon | None = None
        try:
            colon_ast = self.parseColon(var_name_ast)
            colon_ast = cast(AstNode, colon_ast)
        except ParseUnexpectedType:
            pass

        if colon_ast == None:
            semicolon = self.parseSemicolon()
            return AstNodeStatement(var_name_ast.token, var_name_ast, None, None)

        dlog(var_name_ast)
        dlog(colon_ast)

        if len(self.tokens) <= 0: raise ParseEOF(TokenType.EQUAL)
        if tokens[0].typ != TokenType.EQUAL: raise ParseUnexpectedType(TokenType.EQUAL, tokens[0])

        equal = tokens.pop(0)

        dlog("UNIMPLEMENTED")
        exit(1)

        # expr = self.parseExpression()
        # assert expr != None
        # expr = cast(AstNode, expr)

        # # dlog(f"EXPR: {expr}")

        # if isinstance(expr, ParseError):
        #     if expr == "Reached End of File!":
        #         assert isinstance(equal, Token)
        #         self.syntax_error(f"Expected ; but reached EOF", equal)

        # semicolon = tokens.pop(0)
        # if semicolon.typ != TokenType.SEMICOLON:
        #     fatal("We don't support Statements with more than one expressions yet!")

        # return AstNodeStatement(var_name_ast.token, var_name_ast, var_type_ast, expr)

    def parseSemicolon(self) -> Token:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.SEMICOLON)
        if self.tokens[0].typ != TokenType.SEMICOLON: raise ParseUnexpectedType(self.tokens[0], TokenType.SEMICOLON, TokenType.COLON)

        return self.tokens.pop(0)

    def parseColon(self, ident_ast: AstNodeIdentifier) -> AstNodeColon:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.COLON, TokenType.SEMICOLON)
        if self.tokens[0].typ != TokenType.COLON: raise ParseUnexpectedType(self.tokens[0], TokenType.COLON)

        colon: Token = self.tokens.pop(0)

        type_ast: AstNodeIdentifier = self.parseIdentifier()

        return AstNodeColon(colon, ident_ast, type_ast)

    def parseIdentifier(self) -> AstNodeIdentifier:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.IDENT)
        if self.tokens[0].typ != TokenType.IDENT: raise ParseUnexpectedType(self.tokens[0], TokenType.IDENT)
        ident_token = self.tokens.pop(0)
        return AstNodeIdentifier(ident_token, ident_token.lexeme)

    def parseExpression(self) -> AstNode | None:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.IDENT)
        t = self.tokens[0]

        lhs = self.parseName()

        current_node: AstNode = cast(AstNode, lhs)

        # while True:
        #     # Parse the binary operator
        #     binop = self.parseBinOp()
        #     if isinstance(binop, ParseError):
        #         return binop  # Return error if parsing failed

        #     # Parse the next Name (rhs)
        #     rhs = self.parseName()
        #     if isinstance(rhs, ParseError):
        #         return rhs  # Return error if parsing failed

        #     # Create a new AST node for the binary operation
        #     current_node = AstNodeExpression(current_node.token, current_node, binop, rhs)

        #     # Check if there are more tokens to parse
        #     if len(self.tokens) <= 0:
        #         break

        return current_node

    def parseName(self) -> AstNode | None:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.IDENT)
        name = self.parseLiteralValue()
        # return self.parseIdentifier()
        return name

    def parseLiteralValue(self) -> AstNode | None:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.INT)
        t = self.tokens.pop(0)
        if t.typ == TokenType.INT:
            return AstNodeInt(t, int(t.lexeme))
        elif t.typ == TokenType.FLOAT:
            return AstNodeFloat(t, float(t.lexeme))
        elif t.typ == TokenType.STRING:
            return AstNodeString(t, t.lexeme)

        return None

    def parseBinOp(self) -> AstNode | None:
        # TODO: Check if Operator predecence is correct

        arithmeticOp = self.parseArithmeticOp()
        if arithmeticOp != None and arithmeticOp != "Reached End of File!":
            return arithmeticOp

        comparisonOp = self.parseComparisonOp()
        if comparisonOp != None and arithmeticOp != "Reached End of File!":
            return comparisonOp

        logicalOp = self.parseLogicalOp()
        if logicalOp != None and arithmeticOp != "Reached End of File!":
            return logicalOp

        return self.parseBinaryArithmeticOp()

        assert False, "UNREACHABLE!"

    def parseComparisonOp(self) -> AstNode | None:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.GT)
        t = self.tokens.pop(0)

        if t.typ in [ TokenType.GT, TokenType.GTE, TokenType.LT, TokenType.LTE, TokenType.EQUAL_EQUAL, TokenType.NOT_EQUAL ]:
            return AstNodeBinaryOp(t)

        return None

    def parseLogicalOp(self) -> AstNode | None:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.LOGICAL_AND)
        t = self.tokens.pop(0)

        if t.typ in [ TokenType.LOGICAL_AND, TokenType.LOGICAL_OR ]:
            return AstNodeBinaryOp(t)

        return None

    def parseArithmeticOp(self) -> AstNode | None:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.PLUS)
        t = self.tokens.pop(0)

        if t.typ in [ TokenType.PLUS, TokenType.MINUS, TokenType.DIVIDE, TokenType.MODULUS ]:
            return AstNodeBinaryOp(t)

        return None

    def parseBinaryArithmeticOp(self) -> AstNode | None:
        if len(self.tokens) <= 0: raise ParseEOF(TokenType.BINARY_AND)
        t = self.tokens.pop(0)

        if t.typ in [ TokenType.BINARY_AND, TokenType.BINARY_OR, TokenType.BINARY_NOT ]:
            return AstNodeBinaryOp(t)

        return None

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

    print(parser.parse())

if __name__ == '__main__':
    main()
