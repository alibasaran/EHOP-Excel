import std/text/regex
// import iteration1/busy_build_system
import iteration2/better_build_system


fun test(commands : list<string>, expected: string, test-name : string)
  print(test-name ++ ": \n")
  commands.foreach(fn(command) handle-command(command.to-upper()))
  println("Expected: " ++ expected ++ "\n")

fun test-no-deps()
  val test-input = [
    "A1 = 3",
    "Dependencies A1"
  ]
  test(test-input, " ", "Test no deps")

fun test-deps-sum()
  val test-input = [
    "A1 = SUM(A2,A3)",
    "Dependencies A1"
  ]
  test(test-input, "A2 A3", "Test deps sum")

fun test-deps-nested()
  val test-input = [
    "A1 = SUM(A2,SUM(A3,A4))",
    "dependencies A1"
  ]
  test(test-input, "A2 A3 A4", "Test deps nested")

fun test-deps-big-fun()
  val test-input = [
    "D1 = DIV(MUL(A1,10),SUM(MINUS(B1,1),C1))",
    "dependencies D1"
  ]
  test(test-input, "A1 B1 C1", "Test deps big fun")

fun test-dirty-bit-rebuild1()
  val test-input = [
    "A1 = 3",
    "A1",
    "A1",
    "fetch-total"
  ]
  test(test-input, "1", "Test dirty bit single fetch")

fun test-dirty-bit-rebuild-with-fun()
  val test-input = [
    "A1 = 3",
    "A2 = 10",
    "A3 = SUM(A1,A2)",
    "A3",
    "A3",
    "fetch-total"
  ]
  test(test-input, "2", "Test dirty bit single fun")

fun test-dirty-bit-rebuild-with-nested-fun()
  val test-input = [
    "A1 = 1",
    "A2 = 2",
    "A3 = 3",
    "A4 = SUM(A1,SUM(A2,A3))",
    "A4",
    "A4",
    "fetch-total"
  ]
  test(test-input, "3", "Test dirty bit nested fun")

fun test-dirty-bit-rebuild-with-fun-arg-change1()
  val test-input = [
    "A1 = 1", "A2 = 2", "A3 = 3",
    "A4 = SUM(A1,SUM(A2,A3))",
    "A4", // Evaluate once should fetch A1, A2, A3
    "A3 = 4", // Change A3
    "A4", // Re-evaluate A4, A3 should only be fetched
    "fetch-total" // Total is 4
  ]
  test(test-input, "4", "Test dirty bit nested fun with param change 1")

fun test-dirty-bit-rebuild-with-fun-arg-change2()
  val test-input = [
    "A1 = 3",
    "B1 = 2",
    "C1 = 5",
    "D1 = DIV(MUL(A1,10),SUM(MINUS(B1,1),C1))",
    "D1", // 3 fetches here
    "B1 = 6", // Change B1
    "D1", // only B1 should be fetched
    "fetch-total" // total is 4
  ]
  test(test-input, "4", "Test dirty bit nested fun with param change 2")

fun test-dirty-bit-rebuild-with-fun-arg-change3()
  val test-input = [
    "A1 = 3",
    "B1 = 2",
    "B2 = 8",
    "C1 = SUM(A1,B1)",
    "D1 = SUM(B1,B2)" ,
    "E1 = SUM(D1,C1)",
    "E1",
    "A1 = 8",
    "E1"
  ]
  test(test-input, "20", "Test dirty bit nested fun with param change 3")

fun test-same-cell-evaluated-once()
  val test-input = [
    "A1 = 3",
    "B1 = SUM(A1,A1)",
    "B1"
  ]
  test(test-input, "6", "Test dirty bit nested fun with param change 3")


val tests = [
  // test-no-deps,
  // test-deps-sum,
  // test-deps-nested,
  // test-deps-big-fun,
  // test-dirty-bit-rebuild1,
  // test-dirty-bit-rebuild-with-fun,
  // test-dirty-bit-rebuild-with-nested-fun,
  // test-dirty-bit-rebuild-with-fun-arg-change1,
  // test-dirty-bit-rebuild-with-fun-arg-change2,
  test-dirty-bit-rebuild-with-fun-arg-change3,
  //test-same-cell-evaluated-once
]

fun main()
  println("Final printed value is the output")
  tests.foreach(fn (t) handle-app(9, 9, t))