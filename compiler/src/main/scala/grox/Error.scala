package grox

import scala.util.control.NoStackTrace

enum Error extends NoStackTrace:
  case FileNotFound(file: String)
  // case ScannerError
  // case ParserError
