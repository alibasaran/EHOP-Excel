import std/text/regex
// import iteration1/busy_build_system
import src/iteration2/better_build_system


fun test(commands : list<string>, expected: string, test-name : string)
  println("===================")
  print(test-name ++ ": \n")
  commands.foreach(fn(command) ("\t" ++ handle-command(command.to-upper())).println)
  println("Expected: " ++ expected ++ "\n")

fun test_fetch_key()
  val test-input = [
    "A1 = 3",
    "A1"
  ]
  test(test-input, "3", "Test key fetch")

fun test_fun()
  val test-input = [
    "A1 = 3",
    "A2 = 4",
    "A3 = SUM(A1,A2)",
    "A3"
  ]
  test(test-input, "7", "Test fun")

fun test_nested_fun()
  val test-input = [
    "A1 = 3",
    "A2 = 4",
    "A3 = SUM(A1,SUM(A1,A2))",
    "A3"
  ]
  test(test-input, "10", "Test nested fun")

fun test_both_params_fun()
  val test-input = [
    "A1 = 3",
    "A2 = 4",
    "A3 = SUM(SUM(A1,A2),SUM(A1,A2))",
    "A3"
  ]
  test(test-input, "14", "Test both params are fun")

fun test_triple_nested_fun()
  val test-input = [
    "A1 = 3",
    "A2 = 4",
    "A3 = SUM(A1,SUM(A1,SUM(A1,A2)))",
    "A3"
  ]
  test(test-input, "13", "Test triple nested fun")

fun test_div()
  val test-input = [
    "A1 = 6",
    "A2 = 3",
    "A3 = DIV(A1,DIV(A2,3))",
    "A3"
  ]
  test(test-input, "6", "Test fun")

fun test_minus()
  val test-input = [
    "A1 = 6",
    "A2 = 3",
    "A3 = MINUS(A1,A2)",
    "A3"
  ]
  test(test-input, "3", "Test fun")

fun test_mixed_nested_funcs()
  val test-input = [
    "A1 = 3",
    "B1 = 2",
    "C1 = 5",
    "D1 = DIV(MUL(A1,10),SUM(MINUS(B1,1),C1))",
    "D1"
  ]
  test(test-input, "5", "Test mixed nested fun")

fun test-cell-desc()
  val test-input = [
    "D1 = SUM(MUL(A1,10),SUM(MINUS(B1,1),C1))",
    "desc D1"
  ]
  test(test-input, "SUM(MUL(A1,10),SUM(MINUS(B1,1),C1))", "Test cell desc")


val tests = [
  test_fetch_key,
  test_fun,
  test_nested_fun,
  test_triple_nested_fun,
  test_both_params_fun,
  test_div,
  test_minus,
  test_mixed_nested_funcs,
  test-cell-desc
]

fun main()
  tests.foreach(fn (t) handle-app(t))