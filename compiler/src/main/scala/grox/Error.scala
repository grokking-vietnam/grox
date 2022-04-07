package grox

import scala.util.control.NoStackTrace

enum Error extends NoStackTrace:
  case UnexpectedError
  case ScannerError
  case ParserError
