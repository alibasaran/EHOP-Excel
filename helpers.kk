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
  val row = key-encoded.filter(fn(x) x <= 57 && x >= 49).map(fn(x) x - 48).foldl(0, fn(x, y) 10 * x + y)
  val column = key-encoded.filter(fn(x) x > 64).map(fn(x) x - 64).foldl(0, fn(x, y) 25 * x + y)
  (row - 1, column - 1)