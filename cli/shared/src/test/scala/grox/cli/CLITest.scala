package grox.cli

import com.monovore.decline

import CLI.*

class CLITest extends munit.FunSuite:

  def testCommand(args: String*) = decline.Command("test", "Test Command")(CLI.parse).parse(args)

  test("scan option"):
    assertEquals(testCommand("--scan", "."), Right(Config(Command.Scan("."), false)))

  test("parse option"):
    assertEquals(testCommand("--parse", "."), Right(Config(Command.Parse("."), false)))

  test("evaluate option"):
    assertEquals(testCommand("--evaluate", "."), Right(Config(Command.Evaluate("."), false)))

  test("run option"):
    assertEquals(testCommand("--run", "."), Right(Config(CLI.Command.Run("."), false)))

  test("debug flat"):
    assertEquals(
      testCommand("--evaluate", ".", "--debug"),
      Right(Config(Command.Evaluate("."), true)),
    )
