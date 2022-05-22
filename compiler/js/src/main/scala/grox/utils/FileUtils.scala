package grox.utils

object FileUtils:
  def read(path: String) : Either[grox.Error, String] =
    Left(grox.Error.FileNotFound(path))
