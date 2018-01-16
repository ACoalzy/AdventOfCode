package Days

import org.scalatest.FunSuite

class Day4Test extends FunSuite {

  test("unique passphrase returns true for empty phrase") {
    assert(Day4.uniquePassphrase(""))
  }

  test("unique passphrase returns true for single word") {
    assert(Day4.uniquePassphrase("a"))
  }

  test("unique passphrase returns true for multiple distinct words") {
    assert(Day4.uniquePassphrase("a b ccc elephant"))
  }

  test("unique passphrase returns true for multiple distinct words separated by many spaces") {
    assert(Day4.uniquePassphrase("a    b   ef   gggggg"))
  }

  test("unique passphrase returns true for leading and trailing spaces") {
    assert(Day4.uniquePassphrase("    a     b    "))
  }

  test("unique passphrase returns false for duplicate words") {
    assert(Day4.uniquePassphrase("aa bb  cc bb ee") == false)
  }

  test("count unique passphrases returns 0 for empty list") {
    assert(Day4.countUniquePassphrases(Seq()) == 0)
  }

  test("count unique passphrases returns 1 for single element list") {
    assert(Day4.countUniquePassphrases(Seq("a b c d")) == 1)
  }

  test("count unique passphrases returns 4 for 4 valid and 2 invalid phrases") {
    assert(Day4.countUniquePassphrases(Seq("a b c d", "e f g h", "a b a e", "abc def", "abc abc", "testing is the best")) == 4)
  }

  test("no anagram passphrase returns true for empty phrase") {
    assert(Day4.noAnagramPassphrase(""))
  }

  test("no anagram passphrase returns true for single word") {
    assert(Day4.noAnagramPassphrase("a"))
  }

  test("no anagram passphrase returns true for multiple no anagram words") {
    assert(Day4.noAnagramPassphrase("a b ccc elephant"))
  }

  test("no anagram passphrase returns true for multiple distinct words separated by many spaces") {
    assert(Day4.noAnagramPassphrase("a    b   ef   gggggg"))
  }

  test("no anagram passphrase returns true for leading and trailing spaces") {
    assert(Day4.noAnagramPassphrase("    a     b    "))
  }

  test("no anagram passphrase returns false for duplicate words") {
    assert(Day4.noAnagramPassphrase("abcde xyz ecdab") == false)
  }

}
