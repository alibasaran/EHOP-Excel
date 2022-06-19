
pub fun main()
  var str-list := ["A", "B", "C"].vector.ref()
  str-list.modify(fn (local-vec)
    local-vec[1, "A"]
  )
  str-list.foreach(println)
  