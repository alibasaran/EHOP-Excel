import iteration2/hashtable
import std/num/random

fun main()
  var ht : hashtable<int32> := hashtable()
  repeat(10000)
    ht := ht.set(srandom-int32-range(1.int32, 10000.int32).int.show, srandom-int32-range(1.int32, 10000.int32))
  repeat(10000)
    ht.get(srandom-int32-range(1.int32, 10000.int32).int.show)
    ()
