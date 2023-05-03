package grox

import org.scalacheck.{Arbitrary, Gen}

object Arbitraries:

  // todo add Unit
  // probability of string/double should be more than unit
  given Arbitrary[LiteralType] = Arbitrary:
    Gen.oneOf(
      Gen.alphaNumStr,
      Gen.double,
      Gen.long.map(l => l % 2 == 0),
      Gen.const(()),
    )
