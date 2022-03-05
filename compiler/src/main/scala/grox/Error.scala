package grox

sealed trait Error

case class UnexpectedError(exc: Throwable) extends Error
case class ScannerError(err: grox.Scanner.Error) extends Error
