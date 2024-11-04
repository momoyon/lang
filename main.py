import sys
from enum import IntEnum, auto
import pprint

DEBUG = True

def dlog(msg):
    if DEBUG:
        pprint.pp(f"[DEBUG] {msg}")

def error(msg):
    pprint.pp(f"[ERROR] {msg}")

def handle_func(i, tokens):
    def forward(i):
        i += 1
        if i >= len(tokens)-2:
            error(f"Incomplete function!")
        token = tokens[i]
        next  = tokens[i+1]

        return i, token, next

    # ERROR: func is the last or second to last token
    if i >= len(tokens)-2:
        error(f"Incomplete function!")
    token = tokens[i]
    next  = tokens[i+1]
    args = []

    func_name = 'INVALID'
    while i < len(tokens)-2:
        func_name = token.literal_string
        if token.typ == Token_Type.IDENTIFIER:
            # Check if function name is not a keyword
            if token.literal_string in KEYWORDS:
                error(f"Function name cannot be a keyword '{token.literal_string}'")
            break
        i, token, next = forward(i)

    if next.typ != Token_Type.OPEN_PAREN:
        error(f"Expected '(' after function name but got '{next.literal_string}'")
    i, token, next = forward(i)

    dlog(f"Function name '{func_name}'")

    # Get args
    while i < len(tokens)-2:
        if token.typ == Token_Type.CLOSE_PAREN:
            break
        args.append(token)
        i, token, next = forward(i)

    # dlog(f"Function args: '{args}'")

    ret_type = 'void'

    # Get return type if present
    if next.typ == Token_Type.COLON:
        i, token, next = forward(i)
        ret_type = next.literal_string

    dlog(f"Function return type: '{ret_type}'")

    exit(1)
    # assert False, "RAAH"
KEYWORDS = {
        'func': handle_func,
}

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

class Parser:
    def __init__(self, src):
        self.src = src
        self.line = 1
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
        while self.current_char() == '/' and self.next_char() == '/':
            comment = self.consume_comment()
        c = self.current_char()

        if (self.exhausted()):
            return None

        while c.isspace():
            if c == '\n':
                self.next_line()
            else:
                # dlog(f"Skipped '{c}' at line {self.line}:{self.cur - self.bol}")
                self.advance_char()
            c = self.current_char()

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
            error(f"Unexpected char '{c}'. At line {self.line}:{self.cur - self.bol}")
            raise Exception(f"Unexpected char '{c}'. At line {self.line}:{self.cur - self.bol}")

        return None

    def lex(self) -> [Token]:
        tokens = []
        token = self.next_token()
        tokens.append(token)
        while token:
            token = self.next_token()
            tokens.append(token)
        dlog("Done lexing...")
        return tokens

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
    parser = Parser(src)
    tokens = parser.lex()
    # pprint.pp(tokens)

    output_filename = "output"
    output = open(output_filename, 'w')

    # 3. TODO: Syntactical Analysis
    for i in range(0, len(tokens)-1):
        token = tokens[i]
        next  = tokens[i+1]

        if token.typ == Token_Type.IDENTIFIER:
            # assert len(KEYWORDS) == 1, "Every keyword is not handled!"
            if token.literal_string in KEYWORDS:
                func = KEYWORDS[token.literal_string]
                func(i+1, tokens)
                dlog(f"Found keyword: '{token.literal_string}'")
            else:
                dlog(f"Found ident '{token.literal_string}'")
        elif token.typ == Token_Type.OPEN_PAREN:
            dlog(f"Found Open paren")
        elif token.typ == Token_Type.CLOSE_PAREN:
            dlog(f"Found Close paren")
        elif token.typ == Token_Type.COLON:
            dlog(f"Found Colon")
        elif token.typ == Token_Type.COMMA:
            dlog(f"Found Comma")
        elif token.typ == Token_Type.OPEN_SQUARE_BRACE:
            dlog(f"found open square brace")
        elif token.typ == Token_Type.CLOSE_SQUARE_BRACE:
            dlog(f"Found Close square brace")
        elif token.typ == Token_Type.OPEN_BRACE:
            dlog(f"found open brace")
        elif token.typ == Token_Type.CLOSE_BRACE:
            dlog(f"Found Close brace")
        elif token.typ == Token_Type.STRING:
            dlog(f"Found String")
        elif token.typ == Token_Type.SEMICOLON:
            dlog(f"Found Semicolon")
        elif token.typ == Token_Type.NUMBER:
            dlog(f"Found Number")

        else:
            assert False, f"Token_type '{token.type_as_str()}' is unimplemented!"
        # pprint.pp("------------------------------")
        # pprint.pp(f"  Token: {token}")
        # pprint.pp(f"  Next:  {next}")

    output.close()
if __name__ == '__main__':
    main()
