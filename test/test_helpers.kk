import src/helpers

fun test(actual: int, expected: int)
  println("\tExpected: " ++ expected.show ++ "\tGot: " ++ actual.show)

fun test(actual: string, expected : string)
  println("\tExpected: " ++ expected ++ "\tGot: " ++ actual)

fun test-to-key()
  println("Test conversion of integer to keys")
  test(to-key(1), "A")
  test(to-key(26), "Z")
  test(to-key(27), "AA")
  test(to-key(52), "AZ")

fun test-col-to-int()
  println("Test column to integers")
  test(col-to-int("A"), 1)
  test(col-to-int("Z"), 26)
  test(col-to-int("AA"), 27)
  test(col-to-int("AZ"), 52)


fun main()
  // test-idx()
  // test-idx2()
  // test-idx3()
  // test-string-to-float()
  test-to-key()
  test-col-to-int()