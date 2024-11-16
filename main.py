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
    pprint.pp(f"[ERROR] {msg}")

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
    COUNT = auto()

token_type_as_str_map: { TokenType : str } = {
    TokenType.IDENT         : "Ident",
    TokenType.STRING        : "String",
    TokenType.LEFT_PAREN    : "Left paren",
    TokenType.RIGHT_PAREN   : "Right paren",
    TokenType.MINUS         : "Minus",
    TokenType.LEFT_BRACE    : "Left brace",
    TokenType.RIGHT_BRACE   : "Right brace",
    TokenType.RETURNER      : "Returner",
    TokenType.PLUS          : "Plus",
    TokenType.DIVIDE        : "Divide",
    TokenType.MULTIPLY      : "Multiply",
    TokenType.MODULUS       : "Modulus",
    TokenType.EQUAL         : "Equal",
    TokenType.NOT           : "Not",
    TokenType.NOT_EQUAL     : "Not Equal",
    TokenType.EQUAL_EQUAL   : "Equal Equal",
    TokenType.GT            : "Greater Than",
    TokenType.LT            : "Less Than",
    TokenType.GTE           : "Greater Than or Equal",
    TokenType.LTE           : "Less Than or Equal",
}
# NOTE: TokenType.COUNT - 1 because auto() starts from 1
assert len(token_type_as_str_map) == TokenType.COUNT-1

class Token:
    def __init__(self, typ: TokenType, value: str, loc: Loc):
        self.typ = typ
        self.value = value
        self.loc = loc

    def __str__(self):
        return f"Token ({token_type_as_str_map[self.typ]}, '{self.value}', {self.loc})"

class Parser:
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
        else:
            fatal(f"Unrecognized character '{c}'")

        return None

def main():
    program: str = sys.argv.pop(0)

    if len(sys.argv) <= 0:
        error(f"Please provide a filename!")
        usage(program)
        exit(1)

    filename: str = sys.argv.pop(0)

    parser = Parser(filename)

    tokens: [Token] = []
    token = parser.next_token()
    tokens.append(token)
    while token != None:
        token = parser.next_token()
        tokens.append(token)

    for t in tokens:
        pprint.pp(str(t))

if __name__ == '__main__':
    main()
