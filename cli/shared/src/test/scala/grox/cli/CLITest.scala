package grox.cli

import com.monovore.decline

class CLITest extends munit.FunSuite:

  def testCommand(args: String*) = decline.Command("test", "Test Command")(CLI.parse).parse(args)

  test("scan command") {
    assertEquals(testCommand("scan", "."), Right(CLI.Command.Scan(".")))
  }

  test("parse command") {
    assertEquals(testCommand("parse", "."), Right(CLI.Command.Parse(".")))
  }

  test("evaluate command") {
    assertEquals(testCommand("evaluate", "."), Right(CLI.Command.Evaluate(".")))
  }
