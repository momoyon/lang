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
    COUNT = auto()

class Token:
    def __init__(self, typ: TokenType, value: str, loc: Loc):
        self.typ = typ
        self.value = value
        self.loc = loc


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

    def consume_char(self) -> str:
        c = self.current_char()
        self.cur += 1
        return c

    def consume_string(self):
        assert self.current_char() == '"', "Called consume_string() at wrong character!"
        string: str = ''

        self.consume_char()
        c = self.consume_char()
        while c != '"':
            if self.eof():
                self.fatal(f"Unterminated string!")
            string += c
            c = self.consume_char()

        assert self.consume_char() == '"', "This shouldn't happen according to the while loop above"

        return string

    def next_token(self) -> Token | None:
        c = self.current_char()

        t: Token | None = None
        if c == '"':
            t = Token(TokenType.STRING, self.consume_string(), Loc(self.filename, self.line, self.row()))
            pass

        return t
def main():
    program: str = sys.argv.pop(0)

    if len(sys.argv) <= 0:
        error(f"Please provide a filename!")
        usage(program)
        exit(1)

    filename: str = sys.argv.pop(0)

    parser = Parser(filename)

    # token = parser.next_token()


if __name__ == '__main__':
    main()
