from functools import *
# Python, 2017-05-28
# before using this for emt, rename this file to "parser4emt.py"

import unittest

class TestParse(unittest.TestCase):
    def test_parse0(self):
        self.assertEqual(parse("string0"),
                         ["string0"])
    def test_parse1(self):
        self.assertEqual(parse("string0 string1"), 
                         ["string0", "string1"])
    def test_parse2(self):
        self.assertEqual(parse("string0 "),
                         ["string0"])
    def test_parse3(self):
        self.assertEqual(parse("string0 \"string1\""),
                         ["string0", "string1"])
    def test_parse4(self):
        self.assertEqual(parse("string0 \"string1\" string2"),
                         ["string0", "string1", "string2"])
    def test_parse5(self):
        self.assertEqual(parse("string0 \"string1 string1-1\""),
                         ["string0", "string1 string1-1"])
    def test_parse6(self):
        self.assertEqual(parse("string0 \"string1 \\\"string1-1\\\"\""),
                         ["string0", "string1 \"string1-1\""])
    def test_parse7(self):
        self.assertEqual(parse("string0 \"string1 \\\"string1-1\\\"\" string2"),
                         ["string0", "string1 \"string1-1\"", "string2"])

class expr(str):
    def __init__(self, s):
        str.__init__(s)
    def startWith(self, c):
        if type(c) is str:
            return self[0:len(c)] == c
        elif type(c) is list:
            for each_c in c:
                if self[0:len(each_c)] == each_c:
                    return True
            return False
        else:
            raise TypeError("'c' must be string or list")
    def endWith(self, c):
        if type(c) is str:
            return self[-len(c):] == c
        elif type(c) is list:
            for each_c in c:
                if self[-len(each_c):] == each_c:
                    return True
            return False
        else:
            raise TypeError("'c' must be string or list")
    def trim_quote(self):
        return self[1:-1]
    def joinWith(c, expr_list):
        return expr(''.join(reduce(lambda res,content: res + [c] + [content], expr_list, []))[1:])

escape = '\\'
quote = ['\'', '\"']
escaped_quote = ["\\\'", "\\\""]

def car(result):
    return result[-1] if result != [] else None

def proc(_result, piece):
    result = list(_result)
    if piece == '':
        return result
    piece = expr(piece)
    if type(car(result)) == list:
        if piece.endWith(quote) and \
                not piece.endWith(escaped_quote):
            return result[:-1] + [expr.joinWith(' ',result[-1]+[piece])
                                  .trim_quote()
                                  .replace(escaped_quote[0], quote[0])
                                  .replace(escaped_quote[1], quote[1])]
        else:
            return result[:-1] + [result[-1]+[piece]]
    else:
        if piece.startWith(quote):
            if piece.endWith(quote) and \
                    not piece.endWith(escaped_quote):
                return result + [piece.trim_quote()]
            else:
                return result + [[piece]]
        else:
            return result + [piece]

def checkIfValid(s):
    for c in s:
        if type(c) == list:
            raise SyntaxError
        elif escaped_quote[0] in c \
             or escaped_quote[1] in c:
            raise SyntaxError
        elif escape in c:
            raise SyntaxError

def parse(s):
    result = reduce(proc, s.split(), [])
    checkIfValid(result)
    return result
