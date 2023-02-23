package grox

import scala.util.control.NoStackTrace

enum Error extends NoStackTrace:

  case FileNotFound(
    file: String
  )

  override def toString: String =
    this match
      case FileNotFound(file) => s"FileNotFound $file"
