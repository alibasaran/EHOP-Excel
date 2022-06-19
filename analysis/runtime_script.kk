import src/iteration2/better_build_system

fun setup1()
  put("B1", "1")
  put("B2", "SUM(B1,1)")
  put("B3", "MUL(A3,B2)")
  put("C1", "MINUS(B2,B3)")
  put("C2", "SUM(C1,B3)")
  put("C3", "5")
  put("F0", "2")
  put("F1", "1")
  put("F2", "SUM(C3,MUL(C1,B3))")


fun test()
  setup1()
  val calc-chain = [ 
    "A1", "A2", "A3", "B1", "B2", "B3", 
    "C1", "C2", "F0", "F1", "F2", "F3", "F4" 
  ]
  calc-chain.map(eval)
  ()

fun main()
  handle-app(test, False)