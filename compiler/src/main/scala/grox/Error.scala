package grox

sealed trait Error

case class FileError(exc: Throwable) extends Error
case class ScannerError(err: grox.Scanner.Error) extends Error
