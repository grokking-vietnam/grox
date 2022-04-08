package grox.cli

import java.nio.file.Paths

import com.monovore.decline

class CLITest extends munit.FunSuite:

  def testCommand(args: String*) = decline.Command("test", "Test Command")(CLI.parse).parse(args)

  test("scan command") {
    assertEquals(testCommand("scan", "."), Right(CLI.Command.Scan(Paths.get("."))))
  }

  test("parse command") {
    assertEquals(testCommand("parse", "."), Right(CLI.Command.Parse(Paths.get("."))))
  }
