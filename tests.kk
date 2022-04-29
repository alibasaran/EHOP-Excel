import app
import helpers

fun test(res: float64, expected: float64)
  if res != expected then {print("Failed: Expected value: ");print(expected);print(" Actual value: ");print(res);println("")}
  else println("Test passed!")

fun test-app(action, expected: float64)
  test(handle-app(50, 50, action), expected)

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

fun test-put-fetch1()
  println("Test put and fetch:")
  fun action()
    put("A1", Float(1.0))
    fetch("A1")
  test-app(action, 1.0)

fun test-put-fetch2()
  println("Test put and fetch - 2:")
  fun action()
    put("AD1", Float(1.0))
    fetch("AD1")
  test-app(action, 1.0)

fun test-create-task-with-sum-and-fetch()
  println("Test create task with sum func and fetch")
  fun action()
    put("A1", Float(1.0))
    put("D4", Float(1.0))
    put("A3", Task(fn() fetch("A1") + fetch("D4")))
    fetch("A3")
  test-app(action, 2.0)

fun test-put-fetch-with-if-else-conditional()
  println("Test put with fetch and if-else conditional")
  fun action()
    put("A1", Float(1.0))
    put("A3", Float(2.0))
    put("C4", Float(1.0))
    val task-fun = fn() {
      if fetch("C4") == 1.0
      then fetch("A1") + fetch("A3")
      else fetch("A3")
    }
    put("A4", Task(task-fun))
    fetch("A4")
  test-app(action, 3.0)
    
fun main()
  test-put-fetch1()
  test-create-task-with-sum-and-fetch()
  test-put-fetch-with-if-else-conditional()
