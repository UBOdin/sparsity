package sparsity.parser

import fastparse.Parsed

class ParseException(f: Parsed.Failure) extends Exception(f.trace().longMsg)
