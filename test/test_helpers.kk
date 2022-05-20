import helpers

fun test(res: float64, expected: float64)
  if res != expected then {print("  Failed: Expected value: ");print(expected);print(" Actual value: ");print(res);println("")}
  else println("  Test passed!")

fun test-idx()
  println("Test index 1:")
  val res = calculate-index("D4")
  test(float64(res.fst), 3.0)
  test(float64(res.snd), 3.0)

fun test-idx2()
  println("Test index 2:")
  val res = calculate-index("AD1")
  test(float64(res.fst), 0.0)
  test(float64(res.snd), 28.0)

fun test-idx3()
  println("Test index 3:")
  val res = calculate-index("11")
  test(float64(res.fst), 10.0)

fun test-string-to-float()
  println("Test string to float")
  test(string-to-float("10"), 10.0)
  test(string-to-float("0"), 0.0)
  test(string-to-float("12.3"), 12.3)
  test(string-to-float("-12"), -12.0)
  test(string-to-float("-1"), -1.0)


fun main()
  // test-idx()
  // test-idx2()
  // test-idx3()
  test-string-to-float()