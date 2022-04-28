module helpers

/*
Helper functions
*/

pub fun replace-at-index(idx : int, value : a, l : list<a>) : _e list<a>
  // Given an index idx, value and a list replaces the element at idx with value and returns the new list
  val list-split = l.split(idx)
  val tail = list-split.snd.drop(1)
  list-split.fst ++ [value] ++ tail

pub fun string-to-float(s : string) : _e float64
  // Given an integer string converts it into a float
  // Ex. "11" => 11.0
  list(s).map(int).filter(fn(x) x <= 57 && x >= 49).map(fn(x) x - 48).foldl(0, fn(x, y) 10 * x + y).float64

pub fun calculate-index(k: string): _e (int, int)
  // Given a key as input, converts it into a coordinate
  // Ex. A1 => (0, 0) or D2 => (3, 2)
  val key-encoded = list(k).map(int)
  val row = key-encoded.filter(fn(x) x <= 57 && x >= 49).map(fn(x) x - 48).foldl(0, fn(x, y) 10 * x + y)
  val column = key-encoded.filter(fn(x) x > 64).map(fn(x) x - 64).foldl(0, fn(x, y) 25 * x + y)
  (row - 1, column - 1)