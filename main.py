import sys
from enum import IntEnum, auto
import pprint

DEBUG = True

def dlog(msg):
    if DEBUG:
        pprint.pp(f"[DEBUG] {msg}")

class Loc:
    def __init__(self, file, line, col):
        self.filename = file
        self.line = line
        self.col = col

    def __str__(self):
        return f"{self.filename}:{self.line}:{self.col}"

class Token_Type(IntEnum):
    IDENTIFIER         = auto()
    NUMBER             = auto()
    # SYMBOLS
    OPEN_PAREN         = auto()
    CLOSE_PAREN        = auto()
    COMMA              = auto()
    OPEN_SQUARE_BRACE  = auto()
    CLOSE_SQUARE_BRACE = auto()
    COLON              = auto()
    OPEN_BRACE         = auto()
    CLOSE_BRACE        = auto()
    SEMICOLON          = auto()
    HASH               = auto()
    #
    STRING             = auto()
    COUNT              = auto()

assert Token_Type.COUNT == 14, "Check if every symbols are handled here"
symbols = "(),[]{}:;#"

class Token:
    def __init__(self, typ, literal_string):
        self.typ = typ
        self.literal_string = literal_string

    def type_as_str(self):
        assert Token_Type.COUNT == 14, "Every enum value is not handled!"
        if self.typ == Token_Type.IDENTIFIER: return "IDENTIFIER";
        if self.typ == Token_Type.NUMBER: return "NUMBER";
        if self.typ == Token_Type.OPEN_PAREN: return "OPEN_PAREN";
        if self.typ == Token_Type.CLOSE_PAREN: return "CLOSE_PAREN";
        if self.typ == Token_Type.COMMA: return "COMMA";
        if self.typ == Token_Type.OPEN_SQUARE_BRACE: return "OPEN_SQUARE_BRACE";
        if self.typ == Token_Type.CLOSE_SQUARE_BRACE: return "CLOSE_SQUARE_BRACE";
        if self.typ == Token_Type.COLON: return "COLON";
        if self.typ == Token_Type.OPEN_BRACE: return "OPEN_BRACE";
        if self.typ == Token_Type.CLOSE_BRACE: return "CLOSE_BRACE";
        if self.typ == Token_Type.SEMICOLON: return "SEMICOLON";
        if self.typ == Token_Type.HASH: return "HASH";
        if self.typ == Token_Type.STRING: return "STRING";

    def __repr__(self):
        return f"(Token)'{self.literal_string}' ({self.type_as_str()})"

class Parser:
    def __init__(self, filename):
        with open(filename, mode='r') as file:
            self.src = file.read()
        self.bol  = 0 # beginning of line
        self.cur  = 0 # cursor position
        self.loc = Loc(filename, 0, 0)

    def peek_char(self, by=0):
        if (self.cur+by) > len(self.src)-1:
            raise Exception("Exhausted!")
        return self.src[self.cur + by]

    def chop_char(self):
        current_ch = self.peek_char()
        self.cur += 1
        if current_ch.isspace():
            self.bol = self.cur
            self.loc.line += 1
        return current_ch

    def consume_comment(self) -> str:
        assert self.peek_char() == '/' and self.peek_char(1) == '/'
        comment = ''
        # Remove //
        self.chop_char()
        self.chop_char()

        while self.peek_char() != '\n':
            comment += self.peek_char()
            self.chop_char()

        assert self.peek_char() == '\n'
        self.trim_left()

    def consume_identifier(self) -> str:
        c = self.peek_char()
        assert(c.isalpha() or c == '_')

        identifier = ''

        while c.isalnum() or c == '_':
            identifier += c
            c = self.chop_char()
        return identifier

    def consume_number(self) -> str:
        c = self.peek_char()
        assert(c.isdigit())
        number = ''

        while c.isdigit():
            number += c
            c = self.chop_char()

        # dlog(f"Number: '{number}'")
        return number

    def consume_symbol(self) -> str:
        c = self.peek_char()
        assert(c in symbols)
        symbol = c
        self.chop_char()
        return symbol

    def consume_string(self) -> str:
        c = self.peek_char()
        assert(c == '"')
        # TODO: Does the string include the ""s? (for now it doesn't)
        string = ''
        while c != '"':
            string += c
            c = self.chop_char()
        # Remove " at the end
        assert self.peek_char() == '"'
        self.chop_char()

        # dlog(f"String: '{string}'");
        return string

    def exhausted(self) -> bool:
        return self.cur > len(self.src)-1

    def trim_left(self):
        while self.peek_char().isspace():
            self.chop_char()

    def next_token(self) -> bool | Token:
        dlog(str(self.cur))
        self.trim_left()

        if self.peek_char() == '/' and self.peek_char() == '/':
            comment = self.consume_comment()

        c = self.peek_char()

        if c.isalpha() or c == '_':
            return Token(Token_Type.IDENTIFIER, self.consume_identifier())
        elif c.isdigit(): # TODO: Only handles base-10 numbers
            return Token(Token_Type.NUMBER, self.consume_number())
        elif c in symbols:
            symbol = self.consume_symbol()
            token = Token(Token_Type.COUNT, symbol)
            if symbol == "(":
                token.typ = Token_Type.OPEN_PAREN
            elif symbol == ")":
                token.typ = Token_Type.CLOSE_PAREN
            elif symbol == ",":
                token.typ = Token_Type.COMMA
            elif symbol == "[":
                token.typ = Token_Type.OPEN_SQUARE_BRACE
            elif symbol == "]":
                token.typ = Token_Type.CLOSE_SQUARE_BRACE
            elif symbol == ":":
                token.typ = Token_Type.COLON
            elif symbol == "{":
                token.typ = Token_Type.OPEN_BRACE
            elif symbol == "}":
                token.typ = Token_Type.CLOSE_BRACE
            elif symbol == ";":
                token.typ = Token_Type.SEMICOLON
            elif symbol == "#":
                token.typ = Token_Type.HASH
            else:
                raise Exception(f"Unexpected symbol '{symbol}'")

            return token
        elif c == '"':
            return Token(Token_Type.STRING, self.consume_string())
        else:
            raise Exception(f"Unexpected char '{c}'")

        return None

    def lex(self) -> [Token]:
        tokens = []
        token = self.next_token()
        tokens.append(token)
        while token:
            token = self.next_token()
            tokens.append(token)
        return tokens

def main():
    program  = sys.argv.pop(0)
    if (len(sys.argv) <= 0):
        raise Exception("Please provide the filename!")
    filename = sys.argv.pop(0)
    # 1. Source
    parser = Parser(filename)

    # 2. Lexical Analysis
    tokens = parser.lex()
    pprint.pp(tokens)

    # 3. TODO: Syntactical Analysis
    for i in range(0, len(tokens)-1):
        token = tokens[i]
        next  = tokens[i+1]

        # pprint.pp("------------------------------")
        # pprint.pp(f"  Token: {token}")
        # pprint.pp(f"  Next:  {next}")

if __name__ == '__main__':
    main()
