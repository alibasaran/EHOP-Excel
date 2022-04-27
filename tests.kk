import app
import helpers

fun test(res: float64, expected: float64)
  if res != expected then {print("Test failed: Expected value: ");print(expected);print(" Actual value: ");print(res);println("")}
  else println("Test passed!")

fun test-idx()
  val res = calculate-index("D4")
  test(float64(res.fst), 3.0)
  test(float64(res.snd), 3.0)

fun test-idx2()
  val res = calculate-index("AD1")
  test(float64(res.fst), 0.0)
  test(float64(res.snd), 28.0)

fun test-idx3()
  val res = calculate-index("11")
  test(float64(res.fst), 10.0)

fun test-app(action, expected: float64)
  test(handle-app(50, 50, action), expected)

fun test-put-fetch1()
  fun action()
    put("A1", Float(1.0))
    fetch("A1")
  test-app(action, 1.0)

fun test-put-fetch2()
  fun action()
    put("AD1", Float(1.0))
    fetch("AD1")
  test-app(action, 1.0)

fun test-sum()
  fun action()
    put("A1", Float(1.0))
    put("D4", Float(1.0))
    sum("A1", "D4")
  test-app(action, 2.0)

fun main()
  test-put-fetch2()