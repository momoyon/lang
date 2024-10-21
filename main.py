import sys
from enum import IntEnum, auto
import pprint

DEBUG = True

def dlog(msg):
    if DEBUG:
        pprint.pp(f"[DEBUG] {msg}")

class Token_Type(IntEnum):
    IDENTIFIER         = auto()
    NUMBER             = auto()
    OPEN_PAREN         = auto()
    CLOSE_PAREN        = auto()
    COMMA              = auto()
    OPEN_SQUARE_BRACE  = auto()
    CLOSE_SQUARE_BRACE = auto()
    COLON              = auto()
    OPEN_BRACE         = auto()
    CLOSE_BRACE        = auto()
    SEMICOLON          = auto()
    STRING             = auto()
    COUNT              = auto()

class Token:
    def __init__(self, typ, literal_string):
        self.typ = typ
        self.literal_string = literal_string

    def type_as_str(self):
        assert Token_Type.COUNT == 13, "Every enum value is not handled!"
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
        if self.typ == Token_Type.STRING: return "STRING";

    def __repr__(self):
        return f"(Token)'{self.literal_string}' ({self.type_as_str()})"

class Lexer:
    def __init__(self, src):
        self.src = src
        self.line = 0
        self.bol  = 0 # beginning of line
        self.cur  = 0 # cursor position

    def current_char(self):
        if self.cur > len(self.src)-1: return -1
        return self.src[self.cur]

    def next_char(self):
        if self.cur > len(self.src)-2: return -1
        return self.src[self.cur + 1]

    # NOTE: Advances cursor and returns next char, NOT the current char.
    def advance_char(self, by = 1):
        self.cur += by
        return self.current_char()

    def next_line(self):
        c = self.current_char()
        assert(c == '\n')
        while c == '\n':
            c = self.advance_char()
            self.bol = self.cur
            self.line += 1
        # print(f"next_line-> cur: '{c}'{self.cur}")

    def consume_comment(self) -> str:
        c = self.current_char()
        n = self.next_char()
        comment = ''
        if c == '/' and n == '/':
            while c != '\n':
                comment += c
                c = self.advance_char()
            self.next_line()
        else:
            return
        c = self.current_char()
        # print(f"consume_comment-> cur: '{c}'{self.cur}")

        # dlog(f"Comment: '{comment}'")
        return comment

    def consume_identifier(self) -> str:
        c = self.current_char()
        assert(c.isalpha() or c == '_')

        identifier = c
        c = self.advance_char()

        while c.isalnum() or c == '_':
            identifier += c
            c = self.advance_char()
        # dlog(f"Identifier: '{identifier}'")
        return identifier

    def consume_number(self) -> str:
        c = self.current_char()
        assert(c.isdigit())
        number = ''

        while c.isdigit():
            number += c
            c = self.advance_char()

        # dlog(f"Number: '{number}'")
        return number

    def consume_symbol(self) -> str:
        c = self.current_char()
        assert(c in "(),[]{}:;")
        symbol = c
        self.advance_char()
        # dlog(f"Symbol: '{symbol}'")
        return symbol

    def consume_string(self) -> str:
        c = self.current_char()
        assert(c == '"')
        # TODO: Does the string include the ""s? (for now it doesn't)
        string = ''
        c = self.advance_char()
        while c != '"':
            string += c
            c = self.advance_char()
        # Remove " at the end
        self.advance_char()

        # dlog(f"String: '{string}'");
        return string

    def exhausted(self) -> bool:
        return self.cur >= len(self.src)-1

    def next_token(self) -> bool | Token:
        comment = self.consume_comment()
        c = self.current_char()

        # print(f"consume_comment-> cur: '{c}'")
        if (self.exhausted()):
            # dlog(f"cur: {self.cur}, src.len: {len(self.src)}")
            return None

        while c.isspace():
            c = self.advance_char()

        if c.isalpha() or c == '_':
            return Token(Token_Type.IDENTIFIER, self.consume_identifier())
        elif c.isdigit(): # TODO: Only handles base-10 numbers
            return Token(Token_Type.NUMBER, self.consume_number())
        elif c in "(),[]{}:;":
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
            else:
                raise Exception(f"Unexpected symbol '{symbol}'")

            return token
        elif c == '"':
            return Token(Token_Type.STRING, self.consume_string())
        else:
            raise Exception(f"Unexpected char '{c}'")

        return None

def main():
    program  = sys.argv.pop(0)
    if (len(sys.argv) <= 0):
        raise Exception("Please provide the filename!")
    filename = sys.argv.pop(0)
    # 1. Source
    src = ""
    with open(filename, mode='r') as file:
        src = file.read()

    # 2. Lexical Analysis
    lexer = Lexer(src)
    tokens = []
    token = lexer.next_token()
    while token:
        tokens.append(token)
        token = lexer.next_token()

    pprint.pp(tokens)

    # 3. TODO: Syntactical Analysis


if __name__ == '__main__':
    main()
