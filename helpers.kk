module helpers

/*
Helper functions
*/

pub fun replace-at-index(idx : int, value : a, l : list<a>) : _e list<a>
  val list-split = l.split(idx)
  val tail = list-split.snd.drop(1)
  list-split.fst ++ [value] ++ tail

pub fun calculate-index(k: string): _e (int, int)
  val key-encoded = list(k).map(int)
  val row = key-encoded.filter(fn(x) x <= 57 && x > 49).map(fn(x) x - 49).foldl(0, fn(x, y) 10 * x + y)
  val column = key-encoded.filter(fn(x) x >= 65).map(fn(x) x - 65).foldr(0, fn(x, y) 26 * x + y)
  (row, column)