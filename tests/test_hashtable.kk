import iteration2/hashtable
// import iteration2/hashtable_vectored
import std/num/random

// fun main-vectored()
//   val ht : hashtable<int32> = hashtable()
//   repeat(10000)
//     ht.set(srandom-int32-range(1.int32, 10000.int32).int.show, srandom-int32-range(1.int32, 10000.int32))
//     ()
//   repeat(10000)
//     ht.get(srandom-int32-range(1.int32, 10000.int32).int.show)
//     ()

fun main-normal()
  var ht : hashtable<int32> := hashtable()
  repeat(10000)
    ht := ht.set(srandom-int32-range(1.int32, 10000.int32).int.show, srandom-int32-range(1.int32, 10000.int32))
  repeat(10000)
    ht.get(srandom-int32-range(1.int32, 10000.int32).int.show)
    ()

fun main()
  //main-vectored()
  main-normal()

